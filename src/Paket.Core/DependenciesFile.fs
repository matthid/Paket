namespace Paket

open System
open System.IO
open Paket
open Paket.Domain
open Paket.Logging
open Paket.Requirements
open Paket.ModuleResolver
open Paket.PackageResolver
open Paket.PackageSources

/// [omit]
type InstallOptions = 
    { Strict : bool 
      Redirects : bool option
      ResolverStrategyForDirectDependencies : ResolverStrategy option
      ResolverStrategyForTransitives : ResolverStrategy option
      Settings : InstallSettings }

    static member Default = { 
        Strict = false
        Redirects = None
        ResolverStrategyForTransitives = None
        ResolverStrategyForDirectDependencies = None
        Settings = InstallSettings.Default }

type VersionStrategy = {
    VersionRequirement : VersionRequirement
    ResolverStrategy : ResolverStrategy option }

type DependenciesGroup = {
    Name: GroupName
    Sources: PackageSource list 
    Options: InstallOptions
    Packages : PackageRequirement list
    RemoteFiles : UnresolvedSourceFile list
}
    with
        static member New(groupName) =
            { Name = groupName
              Options = InstallOptions.Default
              Sources = []
              Packages = []
              RemoteFiles = [] }

        member this.CombineWith (other:DependenciesGroup) =
            { Name = this.Name
              Options = 
                { Redirects = this.Options.Redirects ++ other.Options.Redirects
                  Settings = this.Options.Settings + other.Options.Settings
                  Strict = this.Options.Strict || other.Options.Strict
                  ResolverStrategyForDirectDependencies = this.Options.ResolverStrategyForDirectDependencies ++ other.Options.ResolverStrategyForDirectDependencies 
                  ResolverStrategyForTransitives = this.Options.ResolverStrategyForTransitives ++ other.Options.ResolverStrategyForTransitives }
              Sources = this.Sources @ other.Sources |> List.distinct
              Packages = this.Packages @ other.Packages
              RemoteFiles = this.RemoteFiles @ other.RemoteFiles }
            
/// [omit]
module DependenciesFileParser = 

    let private basicOperators = ["~>";"==";"<=";">=";"=";">";"<"]
    let private strategyOperators = ['!';'@']
    let private operators =
        basicOperators
        @ (basicOperators |> List.map (fun o -> strategyOperators |> List.map (fun s -> string s + o)) |> List.concat)

    let (|NuGetStrategy|PaketStrategy|NoStrategy|) (text : string) =
        match text |> Seq.tryHead with
        | Some('!') -> NuGetStrategy
        | Some('@') -> PaketStrategy
        | _ -> NoStrategy

    let parseResolverStrategy (text : string) = 
        match text with
        | NuGetStrategy -> Some ResolverStrategy.Min
        | PaketStrategy -> Some ResolverStrategy.Max
        | NoStrategy -> None

    let twiddle(minimum:string) =
        let promote index (values:string array) =
            let parsed, number = Int32.TryParse values.[index]
            if parsed then values.[index] <- (number + 1).ToString()
            if values.Length > 1 then values.[values.Length - 1] <- "0"
            values

        let parts = minimum.Split '.'
        let penultimateItem = Math.Max(parts.Length - 2, 0)
        let promoted = parts |> promote penultimateItem
        String.Join(".", promoted)

    let parseVersionRequirement (text : string) : VersionRequirement =
        try
            let inline parsePrerelease (texts : string list) = 
                match texts |> List.filter ((<>) "") with
                | [] -> PreReleaseStatus.No
                | [x] when String.equalsIgnoreCase x "prerelease" -> PreReleaseStatus.All
                | _ -> PreReleaseStatus.Concrete texts

            if String.IsNullOrWhiteSpace text then VersionRequirement(VersionRange.AtLeast("0"),PreReleaseStatus.No) else

            match text.Split([|' '|],StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
            |  ">=" :: v1 :: "<" :: v2 :: rest -> VersionRequirement(VersionRange.Range(VersionRangeBound.Including,SemVer.Parse v1,SemVer.Parse v2,VersionRangeBound.Excluding),parsePrerelease rest)
            |  ">=" :: v1 :: "<=" :: v2 :: rest -> VersionRequirement(VersionRange.Range(VersionRangeBound.Including,SemVer.Parse v1,SemVer.Parse v2,VersionRangeBound.Including),parsePrerelease rest)
            |  "~>" :: v1 :: ">=" :: v2 :: rest -> VersionRequirement(VersionRange.Range(VersionRangeBound.Including,SemVer.Parse v2,SemVer.Parse(twiddle v1),VersionRangeBound.Excluding),parsePrerelease rest)
            |  "~>" :: v1 :: ">" :: v2 :: rest -> VersionRequirement(VersionRange.Range(VersionRangeBound.Excluding,SemVer.Parse v2,SemVer.Parse(twiddle v1),VersionRangeBound.Excluding),parsePrerelease rest)
            |  ">" :: v1 :: "<" :: v2 :: rest -> VersionRequirement(VersionRange.Range(VersionRangeBound.Excluding,SemVer.Parse v1,SemVer.Parse v2,VersionRangeBound.Excluding),parsePrerelease rest)
            |  ">" :: v1 :: "<=" :: v2 :: rest -> VersionRequirement(VersionRange.Range(VersionRangeBound.Excluding,SemVer.Parse v1,SemVer.Parse v2,VersionRangeBound.Including),parsePrerelease rest)
            | _ -> 
                let splitVersion (text:string) =
                    match basicOperators |> List.tryFind(text.StartsWith) with
                    | Some token -> token, text.Replace(token + " ", "").Split(' ') |> Array.toList
                    | None -> "=", text.Split(' ') |> Array.toList

            
                match splitVersion text with
                | "==", version :: rest -> VersionRequirement(VersionRange.OverrideAll(SemVer.Parse version),parsePrerelease rest)
                | ">=", version :: rest -> VersionRequirement(VersionRange.AtLeast(version),parsePrerelease rest)
                | ">", version :: rest -> VersionRequirement(VersionRange.GreaterThan(SemVer.Parse version),parsePrerelease rest)
                | "<", version :: rest -> VersionRequirement(VersionRange.LessThan(SemVer.Parse version),parsePrerelease rest)
                | "<=", version :: rest -> VersionRequirement(VersionRange.Maximum(SemVer.Parse version),parsePrerelease rest)
                | "~>", minimum :: rest -> VersionRequirement(VersionRange.Between(minimum,twiddle minimum),parsePrerelease rest)
                | _, version :: rest -> VersionRequirement(VersionRange.Exactly(version),parsePrerelease rest)
                | _ -> failwithf "could not parse version range \"%s\"" text
        with
        | _ -> failwithf "could not parse version range \"%s\"" text

    let parseDependencyLine (line:string) =
        let rec parseDepLine start acc =
            if start >= line.Length then acc
            else
                match line.[start] with
                | ' ' -> parseDepLine (start+1) acc
                | '"' ->
                    match line.IndexOf('"', start+1) with
                    | -1  -> failwithf "Unclosed quote in line '%s'" line
                    | ind -> parseDepLine (ind+1) (line.Substring(start+1, ind-start-1)::acc)
                | _ ->
                    match line.IndexOf(' ', start+1) with
                    | -1  -> line.Substring(start)::acc
                    | ind -> parseDepLine (ind+1) (line.Substring(start, ind-start)::acc)

        parseDepLine 0 []
        |> List.rev
        |> List.toArray


    let private parseGitSource trimmed origin originTxt = 
        let parts = parseDependencyLine trimmed
        
        let getParts (projectSpec : string) = 
            match projectSpec.Split [| ':'; '/' |] with
            | [| owner; project |] -> owner, project, None
            | [| owner; project; commit |] -> owner, project, Some commit
            | _ -> failwithf "invalid %s specification:%s     %s" originTxt Environment.NewLine trimmed
        match parts with
        | [| _; projectSpec; fileSpec; authKey |] -> origin, getParts projectSpec, fileSpec, (Some authKey)
        | [| _; projectSpec; fileSpec |] -> origin, getParts projectSpec, fileSpec, None
        | [| _; projectSpec |] -> origin, getParts projectSpec, Constants.FullProjectSourceFileName, None
        | _ -> failwithf "invalid %s specification:%s     %s" originTxt Environment.NewLine trimmed

    let private parseHttpSource trimmed = 
        let parts = parseDependencyLine trimmed
        let removeInvalidChars (str : string) = System.Text.RegularExpressions.Regex.Replace(str, "[:@\,]", "_")
        
        let getParts (projectSpec : string) fileSpec projectName authKey = 
            let projectSpec = projectSpec.TrimEnd('/')
            
            let projectSpec', commit =
                let start = 
                    match projectSpec.IndexOf("://") with
                    | -1 -> 8 // 8 = "https://".Length
                    | pos -> pos + 3
             
                match projectSpec.IndexOf('/', start) with 
                | -1 -> projectSpec, "/"
                | pos -> projectSpec.Substring(0, pos), projectSpec.Substring(pos)
            
            let splitted = projectSpec.TrimEnd('/').Split([| ':'; '/' |], StringSplitOptions.RemoveEmptyEntries)
            
            let fileName = 
                if String.IsNullOrEmpty fileSpec then
                    let name = Seq.last splitted
                    if String.IsNullOrEmpty <| Path.GetExtension(name) then name + ".fs"
                    else name
                else fileSpec
            
            let owner = 
                match projectSpec'.IndexOf("://") with
                | -1 -> projectSpec'
                | pos -> projectSpec'.Substring(pos + 3) |> removeInvalidChars
            
            HttpLink(projectSpec'), (owner, projectName, Some commit), fileName, authKey

        match parseDependencyLine trimmed with
        | [| spec; url |] -> getParts url "" "" None
        | [| spec; url; fileSpec |] -> getParts url fileSpec "" None
        | [| spec; url; fileSpec; authKey |] -> getParts url fileSpec "" (Some authKey)
        | _ -> failwithf "invalid http-reference specification:%s     %s" Environment.NewLine trimmed

    type private ParserOption =
    | ReferencesMode of bool
    | OmitContent of ContentCopySettings
    | FrameworkRestrictions of FrameworkRestrictions
    | AutodetectFrameworkRestrictions
    | ImportTargets of bool
    | CopyLocal of bool
    | CopyContentToOutputDir of CopyToOutputDirectorySettings
    | ReferenceCondition of string
    | Redirects of bool option
    | ResolverStrategyForTransitives of ResolverStrategy option
    | ResolverStrategyForDirectDependencies of ResolverStrategy option

    let private (|Remote|Package|Empty|ParserOptions|SourceFile|Group|) (line:string) =
        match line.Trim() with
        | _ when String.IsNullOrWhiteSpace line -> Empty(line)
        | String.StartsWith "source" _ as trimmed -> Remote(PackageSource.Parse(trimmed))
        | String.StartsWith "group" _ as trimmed -> Group(trimmed.Replace("group ",""))
        | String.StartsWith "nuget" trimmed -> 
            let parts = trimmed.Trim().Replace("\"", "").Split([|' '|],StringSplitOptions.RemoveEmptyEntries) |> Seq.toList

            let isVersion(text:string) = 
                match Int32.TryParse(text.[0].ToString()) with
                | true,_ -> true
                | _ -> false
           
            match parts with
            | name :: operator1 :: version1  :: operator2 :: version2 :: rest
                when List.exists ((=) operator1) operators && List.exists ((=) operator2) operators -> 
                Package(name,operator1 + " " + version1 + " " + operator2 + " " + version2, String.Join(" ",rest))
            | name :: operator :: version  :: rest 
                when List.exists ((=) operator) operators ->
                Package(name,operator + " " + version, String.Join(" ",rest))
            | name :: version :: rest when isVersion version -> 
                Package(name,version,String.Join(" ",rest))
            | name :: rest -> Package(name,">= 0", String.Join(" ",rest))
            | [name] -> Package(name,">= 0","")
            | _ -> failwithf "could not retrieve NuGet package from %s" trimmed
        | String.StartsWith "references" trimmed -> ParserOptions(ParserOption.ReferencesMode(trimmed.Replace(":","").Trim() = "strict"))
        | String.StartsWith "redirects" trimmed ->
            let setting =
                match trimmed.Replace(":","").Trim().ToLowerInvariant() with
                | "on" -> Some true
                | "off" -> Some false
                | _ -> None

            ParserOptions(ParserOption.Redirects(setting))
        | String.StartsWith "strategy" trimmed -> 
            let setting =
                match trimmed.Replace(":","").Trim().ToLowerInvariant() with
                | "max" -> Some ResolverStrategy.Max
                | "min" -> Some ResolverStrategy.Min
                | _ -> None

            ParserOptions(ParserOption.ResolverStrategyForTransitives(setting))
        | String.StartsWith "lowest_matching" trimmed -> 
            let setting =
                match trimmed.Replace(":","").Trim().ToLowerInvariant() with
                | "false" -> Some ResolverStrategy.Max
                | "true" -> Some ResolverStrategy.Min
                | _ -> None

            ParserOptions(ParserOption.ResolverStrategyForDirectDependencies(setting))
        | String.StartsWith "framework" trimmed -> 
            let text = trimmed.Replace(":", "").Trim()
            
            if text = "auto-detect" then 
                ParserOptions(ParserOption.AutodetectFrameworkRestrictions)
            else 
                let restrictions = Requirements.parseRestrictions text
                if String.IsNullOrWhiteSpace text |> not && List.isEmpty restrictions then 
                    failwithf "Could not parse framework restriction \"%s\"" text

                let options = ParserOption.FrameworkRestrictions(FrameworkRestrictionList restrictions)
                ParserOptions options

        | String.StartsWith "content" trimmed -> 
            let setting =
                match trimmed.Replace(":","").Trim().ToLowerInvariant() with
                | "none" -> ContentCopySettings.Omit
                | "once" -> ContentCopySettings.OmitIfExisting
                | _ -> ContentCopySettings.Overwrite

            ParserOptions(ParserOption.OmitContent(setting))
        | String.StartsWith "import_targets" trimmed -> ParserOptions(ParserOption.ImportTargets(trimmed.Replace(":","").Trim() = "true"))
        | String.StartsWith "copy_local" trimmed -> ParserOptions(ParserOption.CopyLocal(trimmed.Replace(":","").Trim() = "true"))
        | String.StartsWith "copy_content_to_output_dir" trimmed -> 
            let setting =
                match trimmed.Replace(":","").Trim().ToLowerInvariant() with
                | "always" -> CopyToOutputDirectorySettings.Always
                | "never" -> CopyToOutputDirectorySettings.Never
                | "preserve_newest" -> CopyToOutputDirectorySettings.PreserveNewest
                | x -> failwithf "Unknown copy_content_to_output_dir settings: %A" x
                        
            ParserOptions(ParserOption.CopyContentToOutputDir(setting))
        | String.StartsWith "condition" trimmed -> ParserOptions(ParserOption.ReferenceCondition(trimmed.Replace(":","").Trim().ToUpper()))
        | String.StartsWith "gist" _ as trimmed ->
            SourceFile(parseGitSource trimmed SingleSourceFileOrigin.GistLink "gist")
        | String.StartsWith "github" _ as trimmed  ->
            SourceFile(parseGitSource trimmed SingleSourceFileOrigin.GitHubLink "github")
        | String.StartsWith "http" _ as trimmed  ->
            SourceFile(parseHttpSource trimmed)
        | String.StartsWith "//" _ -> Empty(line)
        | String.StartsWith "#" _ -> Empty(line)
        | _ -> failwithf "Unrecognized token: %s" line
    
    let parsePackage(sources,parent,name,version,rest:string) =
        let prereleases,optionsText =
            if rest.Contains ":" then
                // boah that's reaaaally ugly, but keeps backwards compat
                let pos = rest.IndexOf ':'
                let s = rest.Substring(0,pos).TrimEnd()
                let pos' = s.LastIndexOf(' ')
                let prereleases = if pos' > 0 then s.Substring(0,pos') else ""
                let s' = if prereleases <> "" then rest.Replace(prereleases,"") else rest
                prereleases,s'
            else
                rest,""

        if operators |> Seq.exists prereleases.Contains || prereleases.Contains("!") then
            failwithf "Invalid prerelease version %s" prereleases

        let packageName = PackageName name
        { Name = packageName
          ResolverStrategyForTransitives = 
            if optionsText.Contains "strategy" then 
                let kvPairs = parseKeyValuePairs optionsText
                match kvPairs.TryGetValue "strategy" with
                | true, "max" -> Some ResolverStrategy.Max 
                | true, "min" -> Some ResolverStrategy.Min
                | _ -> parseResolverStrategy version
            else parseResolverStrategy version 
          ResolverStrategyForDirectDependencies = 
            if optionsText.Contains "lowest_matching" then 
                let kvPairs = parseKeyValuePairs optionsText
                match kvPairs.TryGetValue "lowest_matching" with
                | true, "false" -> Some ResolverStrategy.Max 
                | true, "true" -> Some ResolverStrategy.Min
                | _ -> None
            else None 
          Parent = parent
          Graph = []
          Settings = InstallSettings.Parse(optionsText).AdjustWithSpecialCases packageName
          VersionRequirement = parseVersionRequirement((version + " " + prereleases).Trim(strategyOperators |> Array.ofList)) } 

    let parsePackageLine(sources,parent,line:string) =
        match line with 
        | Package(name,version,rest) -> parsePackage(sources,parent,name,version,rest)
        | _ -> failwithf "Not a package line: %s" line

    let private parseOptions current options =
        match options with 
        | ReferencesMode mode -> { current.Options with Strict = mode } 
        | Redirects mode -> { current.Options with Redirects = mode }
        | ResolverStrategyForTransitives strategy -> { current.Options with ResolverStrategyForTransitives = strategy }
        | ResolverStrategyForDirectDependencies strategy -> { current.Options with ResolverStrategyForDirectDependencies = strategy }
        | CopyLocal mode -> { current.Options with Settings = { current.Options.Settings with CopyLocal = Some mode } }
        | CopyContentToOutputDir mode -> { current.Options with Settings = { current.Options.Settings with CopyContentToOutputDirectory = Some mode } }
        | ImportTargets mode -> { current.Options with Settings = { current.Options.Settings with ImportTargets = Some mode } }
        | FrameworkRestrictions r -> { current.Options with Settings = { current.Options.Settings with FrameworkRestrictions = r } }
        | AutodetectFrameworkRestrictions ->
            { current.Options with Settings = { current.Options.Settings with FrameworkRestrictions = AutoDetectFramework } }
        | OmitContent omit -> { current.Options with Settings = { current.Options.Settings with OmitContent = Some omit } }
        | ReferenceCondition condition -> { current.Options with Settings = { current.Options.Settings with ReferenceCondition = Some condition } }

    let private parseLine fileName (lineNo, state) line =
        match state with
        | current::other ->
            let lineNo = lineNo + 1
            try
                match line with
                | Group(newGroupName) -> lineNo, DependenciesGroup.New(GroupName newGroupName)::current::other
                | Empty(_) -> lineNo, current::other
                | Remote(newSource) -> lineNo, { current with Sources = current.Sources @ [newSource] |> List.distinct }::other
                | ParserOptions(options) ->
                    lineNo,{ current with Options = parseOptions current options} ::other
                | Package(name,version,rest) ->
                    let package = parsePackage(current.Sources,DependenciesFile fileName,name,version,rest)

                    lineNo, { current with Packages = current.Packages @ [package] }::other
                | SourceFile(origin, (owner,project, commit), path, authKey) ->
                    let remoteFile : UnresolvedSourceFile = { Owner = owner; Project = project; Commit = commit; Name = path; Origin = origin; AuthKey = authKey }
                    lineNo, { current with RemoteFiles = current.RemoteFiles @ [remoteFile] }::other
            with
            | exn -> failwithf "Error in paket.dependencies line %d%s  %s" lineNo Environment.NewLine exn.Message
        | [] -> failwithf "Error in paket.dependencies line %d" lineNo

    let parseDependenciesFile fileName lines =
        let groups = 
            lines
            |> Array.fold (parseLine fileName) (0, [DependenciesGroup.New Constants.MainDependencyGroup])
            |> snd
            |> List.rev
            |> List.fold (fun m g ->
                match Map.tryFind g.Name m with
                | Some group -> Map.add g.Name (g.CombineWith group) m
                | None -> Map.add g.Name g m) Map.empty

        fileName, groups, lines
    
    let parseVersionString (version : string) = 
        { VersionRequirement = parseVersionRequirement (version.Trim(strategyOperators |> Array.ofList))
          ResolverStrategy = parseResolverStrategy version }

module DependenciesFileSerializer = 
    let formatVersionRange strategy (versionRequirement : VersionRequirement) : string =
        let prefix = 
            match strategy with
            | Some ResolverStrategy.Min -> "!"
            | Some ResolverStrategy.Max -> "@"
            | None -> ""

        let preReleases = 
            match versionRequirement.PreReleases with
            | No -> ""
            | PreReleaseStatus.All -> "prerelease"
            | Concrete list -> String.Join(" ",list)
            
        let version = 
            match versionRequirement.Range with
            | Minimum x when strategy = None && x = SemVer.Parse "0" -> ""
            | Minimum x -> ">= " + x.ToString()
            | GreaterThan x -> "> " + x.ToString()
            | Specific x when strategy = None -> x.ToString()
            | Specific x -> "= " + x.ToString()
            | VersionRange.Range(_, from, _, _) 
                    when DependenciesFileParser.parseVersionRequirement ("~> " + from.ToString() + preReleases) = versionRequirement -> 
                        "~> " + from.ToString()
            | _ -> versionRequirement.ToString()
            
        let text = prefix + version
        if text <> "" && preReleases <> "" then text + " " + preReleases else text + preReleases

    let sourceString source = "source " + source

    let packageString packageName versionRequirement resolverStrategy (settings:InstallSettings) =
        let version = formatVersionRange resolverStrategy versionRequirement
        let s = settings.ToString()

        sprintf "nuget %O%s%s" packageName (if version <> "" then " " + version else "") (if s <> "" then " " + s else s)


/// Allows to parse and analyze paket.dependencies files.
type DependenciesFile(fileName,groups:Map<GroupName,DependenciesGroup>, textRepresentation:string []) =
    let tryMatchPackageLine packageNamePredicate (line : string) =
        let tokens = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> s.ToLowerInvariant().Trim())
        match List.ofArray tokens with
        | "nuget"::packageName::_ when packageNamePredicate packageName -> Some packageName
        | _ -> None
    let isPackageLine name line = tryMatchPackageLine ((=) name) line |> Option.isSome

    let findGroupBorders groupName = 
        let _,_,firstLine,lastLine =
            textRepresentation
            |> Array.fold (fun (i,currentGroup,firstLine,lastLine) line -> 
                    if line.TrimStart().StartsWith "group " then
                        let group = line.Replace("group","").Trim()
                        if currentGroup = groupName then
                            i+1,GroupName group,firstLine,(i - 1)
                        else
                            if GroupName group = groupName then
                                i+1,GroupName group,(i + 1),lastLine
                            else
                                i+1,GroupName group,firstLine,lastLine
                    else
                        i+1,currentGroup,firstLine,lastLine)
                (0,Constants.MainDependencyGroup,0,textRepresentation.Length)
        firstLine,lastLine

    let tryFindPackageLine groupName (packageName:PackageName) =
        let name = packageName.GetCompareString()
        let _,_,found =
            textRepresentation
            |> Array.fold (fun (i,currentGroup,found) line -> 
                    match found with
                    | Some _ -> i+1,currentGroup,found
                    | None ->
                        if currentGroup = groupName && isPackageLine name line then
                            i+1,currentGroup,Some i
                        else
                            if line.StartsWith "group " then
                                let group = line.Replace("group","").Trim()
                                i+1,GroupName group,found
                            else
                                i+1,currentGroup,found)
                (0,Constants.MainDependencyGroup,None)
        found

    /// Returns all direct NuGet dependencies in the given group.
    member __.GetDependenciesInGroup(groupName:GroupName) =
        match groups |> Map.tryFind groupName with
        | None -> failwithf "Group %O doesn't exist in the paket.dependencies file." groupName
        | Some group ->
            group.Packages 
            |> Seq.map (fun p -> p.Name, p.VersionRequirement)
            |> Map.ofSeq

    member __.Groups = groups

    member this.SimplifyFrameworkRestrictions() = 
        let transform (dependenciesFile:DependenciesFile) (group:DependenciesGroup) =
            if group.Options.Settings.FrameworkRestrictions |> getRestrictionList <> [] then dependenciesFile else
            match group.Packages with
            | [] -> dependenciesFile
            | package::rest ->
                let commonRestrictions =
                    package.Settings.FrameworkRestrictions
                    |> getRestrictionList
                    |> List.filter (fun r -> rest |> Seq.forall (fun p' -> p'.Settings.FrameworkRestrictions |> getRestrictionList |> List.contains r))

                match commonRestrictions with
                | [] -> dependenciesFile
                | _ ->
                    let newDependenciesFile = dependenciesFile.AddFrameworkRestriction(group.Name,commonRestrictions)
                    group.Packages
                     |> List.fold (fun (d:DependenciesFile) package ->
                            let oldRestrictions = package.Settings.FrameworkRestrictions |> getRestrictionList
                            let newRestrictions = oldRestrictions |> List.filter (fun r -> commonRestrictions |> List.contains r |> not)
                            if oldRestrictions = newRestrictions then d else
                            let (d:DependenciesFile) = d.Remove(group.Name,package.Name)
                            let installSettings = { package.Settings with FrameworkRestrictions = FrameworkRestrictionList newRestrictions }
                            let vr = { VersionRequirement = package.VersionRequirement; ResolverStrategy = package.ResolverStrategyForDirectDependencies }
                            d.AddAdditionalPackage(group.Name, package.Name,vr,installSettings)) newDependenciesFile

        this.Groups
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.fold transform this

    member this.GetGroup groupName =
        match this.Groups |> Map.tryFind groupName with
        | Some g -> g
        | None -> failwithf "Group %O was not found in %s." groupName fileName

    member __.HasPackage (groupName, name : PackageName) = 
        match groups |> Map.tryFind groupName with
        | None -> false
        | Some g -> g.Packages |> List.exists (fun p -> p.Name = name)

    member __.GetPackage (groupName, name : PackageName) = groups.[groupName].Packages |> List.find (fun p -> p.Name = name)
    member __.FileName = fileName
    member __.Lines = textRepresentation

    member this.Resolve(force, getSha1, getVersionF, getPackageDetailsF, groupsToResolve:Map<GroupName,_>, updateMode) =
        let resolveGroup groupName _ =
            let group = this.GetGroup groupName

            let resolveSourceFile (file:ResolvedSourceFile) : (PackageRequirement list * UnresolvedSourceFile list) =
                let remoteDependenciesFile =
                    RemoteDownload.downloadDependenciesFile(force,Path.GetDirectoryName fileName, groupName, DependenciesFile.FromCode, file)
                    |> Async.RunSynchronously

                // We do not support groups in reference files yet
                let mainGroup = remoteDependenciesFile.Groups.[Constants.MainDependencyGroup]
                mainGroup.Packages,mainGroup.RemoteFiles

            let remoteFiles = ModuleResolver.Resolve(resolveSourceFile,getSha1,group.RemoteFiles)
        
            let remoteDependencies = 
                remoteFiles
                |> List.map (fun f -> f.Dependencies)
                |> List.fold Set.union Set.empty
                |> Seq.map (fun (n, v) -> 
                        { Name = n
                          VersionRequirement = v
                          ResolverStrategyForDirectDependencies = Some ResolverStrategy.Max
                          ResolverStrategyForTransitives = Some ResolverStrategy.Max
                          Parent = PackageRequirementSource.DependenciesFile fileName
                          Graph = []
                          Settings = group.Options.Settings })
                |> Seq.toList

            let resolution =
                PackageResolver.ResolveNew(
                    group.Sources,
                    getVersionF groupName, 
                    getPackageDetailsF, 
                    group.Options.ResolverStrategyForDirectDependencies,
                    group.Options.ResolverStrategyForTransitives,
                    group.Options.Settings.FrameworkRestrictions,
                    remoteDependencies @ group.Packages |> Set.ofList,
                    updateMode)

            { ResolvedPackages = resolution
              ResolvedSourceFiles = remoteFiles }

        groupsToResolve
        |> Map.map resolveGroup 


    member private this.AddFrameworkRestriction(groupName, frameworkRestrictions:FrameworkRestriction list) =
        if frameworkRestrictions = [] then this else
        let restrictionString = sprintf "framework %s" (String.Join(", ",frameworkRestrictions))

        let list = new System.Collections.Generic.List<_>()
        list.AddRange textRepresentation

        match groups |> Map.tryFind groupName with 
        | None -> list.Add(restrictionString)
        | Some group ->
            let firstGroupLine,_ = findGroupBorders groupName
            let pos = ref firstGroupLine
            while list.Count > !pos && list.[!pos].TrimStart().StartsWith "source" do
                pos := !pos + 1

            list.Insert(!pos,restrictionString)
       
        DependenciesFile(
            list 
            |> Seq.toArray
            |> DependenciesFileParser.parseDependenciesFile fileName)

    member __.AddAdditionalPackage(groupName, packageName:PackageName,versionRequirement,resolverStrategy,settings,?pinDown) =
        let pinDown = defaultArg pinDown false
        let packageString = DependenciesFileSerializer.packageString packageName versionRequirement resolverStrategy settings

        // Try to find alphabetical matching position to insert the package
        let isPackageInLastSource (p:PackageRequirement) =
            match groups |> Map.tryFind groupName with
            | None -> true
            | Some group ->
                match group.Sources with
                | [] -> true
                | sources -> 
                    let lastSource = Seq.last sources
                    group.Sources |> Seq.exists (fun s -> s = lastSource)

        let smaller = 
            match groups |> Map.tryFind groupName with
            | None -> []
            | Some group ->
                group.Packages 
                |> Seq.takeWhile (fun (p:PackageRequirement) -> p.Name <= packageName || not (isPackageInLastSource p)) 
                |> List.ofSeq
        
        let list = new System.Collections.Generic.List<_>()
        list.AddRange textRepresentation
        let newGroupInserted =
            match groups |> Map.tryFind groupName with
            | None -> 
                if list.Count > 0 then
                    list.Add("")
                list.Add(sprintf "group %O" groupName)
                list.Add(DependenciesFileSerializer.sourceString Constants.DefaultNuGetStream)
                list.Add("")
                true
            | _ -> false

        match tryFindPackageLine groupName packageName with
        | Some pos -> 
            let package = DependenciesFileParser.parsePackageLine(groups.[groupName].Sources,PackageRequirementSource.DependenciesFile fileName,list.[pos])

            if versionRequirement.Range.IsIncludedIn(package.VersionRequirement.Range) then
                list.[pos] <- packageString
            else
                list.Insert(pos + 1, packageString)
        | None -> 
            let firstGroupLine,lastGroupLine = findGroupBorders groupName
            if pinDown then
                if newGroupInserted then
                    list.Add(packageString)
                else
                    list.Insert(lastGroupLine, packageString)
            else
                match smaller with
                | [] -> 
                    match groups |> Map.tryFind groupName with 
                    | None -> list.Add(packageString)
                    | Some group ->
                        match group.Packages with
                        | [] ->
                            if group.RemoteFiles <> [] then
                                list.Insert(firstGroupLine,"")
                    
                            match group.Sources with
                            | [] -> 
                                list.Insert(firstGroupLine,packageString)
                                list.Insert(firstGroupLine,"")
                                list.Insert(firstGroupLine,DependenciesFileSerializer.sourceString Constants.DefaultNuGetStream)
                            | _ -> list.Insert(lastGroupLine, packageString)
                        | p::_ -> 
                            match tryFindPackageLine groupName p.Name with
                            | None -> list.Add packageString
                            | Some pos -> list.Insert(pos,packageString)
                | _ -> 
                    let p = Seq.last smaller

                    match tryFindPackageLine groupName p.Name with
                    | None -> list.Add packageString
                    | Some found -> 
                        let pos = ref (found + 1)
                        let skipped = ref false
                        while !pos < textRepresentation.Length - 1 && (String.IsNullOrWhiteSpace textRepresentation.[!pos] || String.startsWithIgnoreCase "source" textRepresentation.[!pos]) do
                            if String.startsWithIgnoreCase "source" textRepresentation.[!pos] then
                                skipped := true
                            pos := !pos + 1
                            
                        if !skipped then
                            list.Insert(!pos,packageString)
                        else
                            list.Insert(found + 1,packageString)
        
        DependenciesFile(
            list 
            |> Seq.toArray
            |> DependenciesFileParser.parseDependenciesFile fileName)


    member this.AddAdditionalPackage(groupName, packageName:PackageName,version:string,settings) =
        let vr = DependenciesFileParser.parseVersionString version

        this.AddAdditionalPackage(groupName, packageName,vr,settings)

    member this.AddAdditionalPackage(groupName, packageName:PackageName,vr:VersionStrategy,settings) =
        this.AddAdditionalPackage(groupName, packageName,vr.VersionRequirement,vr.ResolverStrategy,settings)

    member this.AddFixedPackage(groupName, packageName:PackageName,version:string,settings) =
        let vr = DependenciesFileParser.parseVersionString version

        let resolverStrategy,versionRequirement = 
            match groups |> Map.tryFind groupName with
            | None -> vr.ResolverStrategy,vr.VersionRequirement
            | Some group ->
                match group.Packages |> List.tryFind (fun p -> p.Name = packageName) with
                | Some package -> 
                    package.ResolverStrategyForTransitives,
                    match package.VersionRequirement.Range with
                    | OverrideAll(_) -> package.VersionRequirement
                    | _ -> vr.VersionRequirement
                | None -> vr.ResolverStrategy,vr.VersionRequirement

        this.AddAdditionalPackage(groupName, packageName,versionRequirement,resolverStrategy,settings,true)

    member this.AddFixedPackage(groupName, packageName:PackageName,version:string) =
        this.AddFixedPackage(groupName, packageName,version,InstallSettings.Default)

    member this.RemovePackage(groupName, packageName:PackageName) =
        match tryFindPackageLine groupName packageName with 
        | None -> this
        | Some pos ->
            let removeElementAt index myArr =
                [|  for i = 0 to Array.length myArr - 1 do 
                       if i <> index then yield myArr.[ i ] |]

            let newLines = removeElementAt pos textRepresentation
            DependenciesFile(DependenciesFileParser.parseDependenciesFile fileName newLines)

    member this.Add(groupName, packageName,version:string,?installSettings : InstallSettings) =
        let installSettings = defaultArg installSettings InstallSettings.Default
        if this.HasPackage(groupName, packageName) && String.IsNullOrWhiteSpace version then 
            traceWarnfn "%s contains package %O in group %O already. ==> Ignored" fileName packageName groupName
            this
        else
            if version = "" then
                tracefn "Adding %O to %s into group %O" packageName fileName groupName
            else
                tracefn "Adding %O %s to %s into group %O" packageName version fileName groupName
            this.AddAdditionalPackage(groupName, packageName,version,installSettings)

    member this.Remove(groupName, packageName) =
        if this.HasPackage(groupName, packageName) then
            tracefn "Removing %O from %s (group %O)" packageName fileName groupName
            this.RemovePackage(groupName, packageName)
        else
            traceWarnfn "%s doesn't contain package %O in group %O. ==> Ignored" fileName packageName groupName
            this

    member this.UpdatePackageVersion(groupName, packageName, version:string) = 
        if this.HasPackage(groupName,packageName) then
            let vr = DependenciesFileParser.parseVersionString version

            tracefn "Updating %O to version %s in %s group %O" packageName version fileName groupName
            let newLines = 
                this.Lines 
                |> Array.map (fun l -> 
                    let name = packageName.GetCompareString()
                    if isPackageLine name l then 
                        let p = this.GetPackage(groupName,packageName)
                        DependenciesFileSerializer.packageString packageName vr.VersionRequirement vr.ResolverStrategy p.Settings
                    else l)

            DependenciesFile(DependenciesFileParser.parseDependenciesFile this.FileName newLines)
        else 
            traceWarnfn "%s doesn't contain package %O in group %O. ==> Ignored" fileName packageName groupName
            this

    member this.UpdateFilteredPackageVersion(groupName, packageFilter: PackageFilter, version:string) =
        let vr = DependenciesFileParser.parseVersionString version

        tracefn "Updating %O to version %s in %s group %O" packageFilter version fileName groupName
        let newLines =
            this.Lines
            |> Array.map (fun l ->
                match tryMatchPackageLine (PackageName >> packageFilter.Match) l with
                | Some matchedName ->
                    let matchedPackageName = PackageName matchedName
                    let p = this.GetPackage(groupName,matchedPackageName)
                    DependenciesFileSerializer.packageString matchedPackageName vr.VersionRequirement vr.ResolverStrategy p.Settings
                | None -> l)

        DependenciesFile(DependenciesFileParser.parseDependenciesFile this.FileName newLines)

    member this.RootPath = FileInfo(fileName).Directory.FullName

    override __.ToString() = String.Join(Environment.NewLine, textRepresentation |> Array.skipWhile String.IsNullOrWhiteSpace)

    member this.Save() =
        File.WriteAllText(fileName, this.ToString())
        tracefn "Dependencies files saved to %s" fileName

    static member FromCode(rootPath,code:string) : DependenciesFile = 
        DependenciesFile(DependenciesFileParser.parseDependenciesFile (Path.Combine(rootPath,Constants.DependenciesFileName)) <| code.Replace("\r\n","\n").Replace("\r","\n").Split('\n'))

    static member FromCode(code:string) : DependenciesFile = 
        DependenciesFile(DependenciesFileParser.parseDependenciesFile "" <| code.Replace("\r\n","\n").Replace("\r","\n").Split('\n'))

    static member ReadFromFile fileName : DependenciesFile = 
        verbosefn "Parsing %s" fileName
        DependenciesFile(DependenciesFileParser.parseDependenciesFile fileName <| File.ReadAllLines fileName)

    /// Find the matching lock file to a dependencies file
    static member FindLockfile(dependenciesFileName) =
        FileInfo(Path.Combine(FileInfo(dependenciesFileName).Directory.FullName, Constants.LockFileName))

    /// Find the matching lock file to a dependencies file
    member this.FindLockfile() = DependenciesFile.FindLockfile this.FileName