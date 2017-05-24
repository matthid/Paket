/// Contains logic which helps to resolve the dependency graph.
module Paket.PackageResolver

open Paket
open Paket.Domain
open Paket.Requirements
open Paket.Logging
open System.Collections.Generic
open System
open System.Diagnostics
open Paket.PackageSources

type DependencySet = Set<PackageName * VersionRequirement * FrameworkRestrictions>

/// Represents package details
[<StructuredFormatDisplay "{Display}">]
type PackageDetails = {
    Name               : PackageName
    Source             : PackageSource
    DownloadLink       : string
    LicenseUrl         : string
    Unlisted           : bool
    DirectDependencies : DependencySet
}

/// Represents data about resolved packages
[<StructuredFormatDisplay "{Display}">]
type ResolvedPackage = {
    Name                : PackageName
    Version             : SemVerInfo
    Dependencies        : DependencySet
    Unlisted            : bool
    IsRuntimeDependency : bool
    Settings            : InstallSettings
    Source              : PackageSource
} with
    override this.ToString () = sprintf "%O %O" this.Name this.Version

    member self.HasFrameworkRestrictions =
        getExplicitRestriction self.Settings.FrameworkRestrictions <> FrameworkRestriction.NoRestriction

    member private self.Display
        with get() =
            let deps =
                self.Dependencies
                |> Seq.map (fun (name,ver,restrict) ->
                    sprintf "  <%A - %A - %A>\n" name ver restrict)
                |> String.Concat
            sprintf
                "%A\nDependencies -\n%s\nSource - %A\nInstall Settings\n%A"
                    self.Name deps self.Source self.Settings

type PackageResolution = Map<PackageName, ResolvedPackage>

module DependencySetFilter =
    let isIncluded (restriction:FrameworkRestriction) (dependency:PackageName * VersionRequirement * FrameworkRestrictions) =
        let _,_,dependencyRestrictions = dependency
        let dependencyRestrictions = dependencyRestrictions |> getExplicitRestriction
        if dependencyRestrictions = FrameworkRestriction.NoRestriction then true else
        // While the dependency specifies the framework restrictions of the dependency ([ >= netstandard13 ])
        // we need to take the dependency, when the combination still contains packages.
        // NOTE: This is not forwards compatible...
        //let combined = FrameworkRestriction.And [ restriction; dependencyRestrictions ]
        //not combined.RepresentedFrameworks.IsEmpty

        // "And" is not cheap therefore we use this, 
        // because we don't want to pay the price of calculating a "simplified" formula 
        Set.intersect restriction.RepresentedFrameworks dependencyRestrictions.RepresentedFrameworks
        |> Set.isEmpty
        |> not

    let filterByRestrictions (restrictions:FrameworkRestrictions) (dependencies:DependencySet) : DependencySet =
        match getExplicitRestriction restrictions with
        | FrameworkRestriction.HasNoRestriction -> dependencies
        | restrictions ->
            dependencies
            |> Set.filter (isIncluded restrictions)

    let findIncompatibleDependencies (dependencies:DependencySet) (package:ResolvedPackage) =
        dependencies
        // exists any not matching stuff
        |> Seq.filter (fun (name, requirement, restriction) ->
            if name = package.Name then
                requirement.IsInRange package.Version |> not
            else false)


let cleanupNames (model : PackageResolution) : PackageResolution =
    model
    |> Map.map (fun _ package ->
        { package with
            Dependencies =
                package.Dependencies
                |> Set.map (fun (name, v, d) -> model.[name].Name, v, d) })


type ResolverStep = {
    Relax: bool
    FilteredVersions : Map<PackageName, ((SemVerInfo * PackageSource list) list * bool)>
    CurrentResolution : Map<PackageName,ResolvedPackage>;
    ClosedRequirements : Set<PackageRequirement>
    OpenRequirements : Set<PackageRequirement> }


[<RequireQualifiedAccess>]
[<DebuggerDisplay "{DebugDisplay()}">]
type Resolution =
| Ok of PackageResolution
| Conflict of resolveStep    : ResolverStep
            * requirementSet : PackageRequirement Set
            * requirement    : PackageRequirement
            * getPackageVersions : (PackageName -> (SemVerInfo * PackageSource list) seq)
    member private self.DebugDisplay() =
        match self with
        | Ok pkgres ->   
            pkgres |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
            |> Array.ofSeq |> sprintf "Ok - %A"
        | Conflict (resolveStep,reqSet,req,_) ->
            sprintf "%A\n%A\n%A\n" resolveStep reqSet req


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Resolution =

    open System.Text

    let getConflicts (res:Resolution) =
        match res with
        | Resolution.Ok _ -> Set.empty
        | Resolution.Conflict (currentStep,_,lastPackageRequirement,_) ->
            currentStep.ClosedRequirements
            |> Set.union currentStep.OpenRequirements
            |> Set.add lastPackageRequirement
            |> Set.filter (fun x -> x.Name = lastPackageRequirement.Name)

    let buildConflictReport (errorReport:StringBuilder)  (conflicts:PackageRequirement Set) =
        let formatVR (vr:VersionRequirement) =
            vr.ToString ()
            |> fun s -> if String.IsNullOrWhiteSpace s then ">= 0" else s

        let formatPR hasPrereleases (vr:VersionRequirement) = 
            if hasPrereleases && vr.PreReleases = PreReleaseStatus.No then " (no prereleases)" else
            match vr.PreReleases with
            | PreReleaseStatus.Concrete [x] -> sprintf " (%s)" x
            | PreReleaseStatus.Concrete x -> sprintf " %A" x
            | _ -> ""

        match conflicts with
        | s when s.IsEmpty -> errorReport
        | conflicts ->
            let hasPrereleases = Seq.exists (fun r -> r.VersionRequirement.PreReleases <> PreReleaseStatus.No) conflicts

            errorReport.AddLine (sprintf "  Conflict detected:")

            let getConflictMessage req =
                let vr = formatVR req.VersionRequirement
                let pr = formatPR hasPrereleases req.VersionRequirement
                match req.Parent with
                | DependenciesFile _ ->
                    sprintf "   - Dependencies file requested package %O: %s%s" req.Name vr pr
                | Package (parentName,version,_) ->
                    sprintf "   - %O %O requested package %O: %s%s" parentName version req.Name vr pr
            
            conflicts
            |> Seq.fold (fun (errorReport:StringBuilder) conflict ->
                errorReport.AppendLine (getConflictMessage conflict)) errorReport

    let getErrorText showResolvedPackages = function
    | Resolution.Ok _ -> ""
    | Resolution.Conflict (currentStep,_,_,getVersionF) as res ->
        let errorText =
            if showResolvedPackages && not currentStep.CurrentResolution.IsEmpty then
                ( StringBuilder().AppendLine  "  Resolved packages:"
                , currentStep.CurrentResolution)
                ||> Map.fold (fun sb _ resolvedPackage ->
                    sb.AppendLinef "   - %O %O" resolvedPackage.Name resolvedPackage.Version)
            else StringBuilder()

        match getConflicts res with
        | c when c.IsEmpty  ->
            errorText.AppendLinef
                "  Could not resolve package %O. Unknown resolution error."
                    (Seq.head currentStep.OpenRequirements)
        | cfs when cfs.Count = 1 ->
            let c = cfs.MinimumElement
            let errorText = buildConflictReport errorText cfs
            match getVersionF c.Name |> Seq.toList with
            | [] -> errorText.AppendLinef  "   - No versions available."
            | avalaibleVersions ->
                (errorText.AppendLinef  "   - Available versions:" , avalaibleVersions)
                ||> List.fold (fun sb elem -> sb.AppendLinef "     - %O" elem)
        | conflicts -> buildConflictReport errorText conflicts
        |> string


    let getModelOrFail = function
    | Resolution.Ok model -> model
    | Resolution.Conflict _ as res ->
        failwithf  "There was a version conflict during package resolution.\n\
                    %s\n  Please try to relax some conditions or resolve the conflict manually (see http://fsprojects.github.io/Paket/nuget-dependencies.html#Use-exactly-this-version-constraint)." (getErrorText true res)


    let isDone = function
    | Resolution.Ok _ -> true
    | _ -> false

type Resolution with

    member self.GetConflicts () = Resolution.getConflicts self
    member self.GetErrorText showResolvedPackages = Resolution.getErrorText showResolvedPackages self
    member self.GetModelOrFail () = Resolution.getModelOrFail self
    member self.IsDone = Resolution.isDone self


let calcOpenRequirements (exploredPackage:ResolvedPackage,globalFrameworkRestrictions,(versionToExplore,_),dependency,resolverStep:ResolverStep) =
    let dependenciesByName =
        // there are packages which define multiple dependencies to the same package
        // we compress these here - see #567
        let dict = Dictionary<_,_>()
        exploredPackage.Dependencies
        |> Set.iter (fun ((name,v,r) as dep) ->
            match dict.TryGetValue name with
            | true,(_,v2,r2) ->
                match v,v2 with
                | VersionRequirement(ra1,p1),VersionRequirement(ra2,p2) when p1 = p2 ->
                    let newRestrictions =
                        match r with
                        | ExplicitRestriction r ->
                            match r2 with
                            | ExplicitRestriction r2 ->
                                FrameworkRestriction.combineRestrictionsWithOr r r2 |> ExplicitRestriction
                            | AutoDetectFramework -> ExplicitRestriction r
                        | AutoDetectFramework -> r

                    if ra1.IsIncludedIn ra2 then
                        dict.[name] <- (name,v,newRestrictions)
                    elif ra2.IsIncludedIn ra1 then
                        dict.[name] <- (name,v2,newRestrictions)
                    else dict.[name] <- dep
                | _ ->  dict.[name] <- dep
            | _ -> dict.Add(name,dep))
        dict
        |> Seq.map (fun kv -> kv.Value)
        |> Set.ofSeq

    let rest =
        resolverStep.OpenRequirements
        |> Set.remove dependency

    dependenciesByName
    |> Set.map (fun (n, v, restriction) ->
        let newRestrictions =
            filterRestrictions restriction exploredPackage.Settings.FrameworkRestrictions
            |> filterRestrictions globalFrameworkRestrictions
            |> fun xs -> if xs = ExplicitRestriction FrameworkRestriction.NoRestriction then exploredPackage.Settings.FrameworkRestrictions else xs

        { dependency with
            Name = n
            VersionRequirement = v
            Parent = Package(dependency.Name, versionToExplore, exploredPackage.Source)
            Graph = Set.add dependency dependency.Graph
            Settings = { dependency.Settings with FrameworkRestrictions = newRestrictions } })
    |> Set.filter (fun d ->
        resolverStep.ClosedRequirements
        |> Set.exists (fun x ->
            x.Name = d.Name &&
               x.Settings.FrameworkRestrictions = d.Settings.FrameworkRestrictions &&
                (x = d ||
                 x.VersionRequirement.Range.IsIncludedIn d.VersionRequirement.Range ||
                 x.VersionRequirement.Range.IsGlobalOverride))
        |> not)
    |> Set.filter (fun d ->
        resolverStep.OpenRequirements
        |> Set.exists (fun x -> x.Name = d.Name && (x = d || x.VersionRequirement.Range.IsGlobalOverride) && x.Settings.FrameworkRestrictions = d.Settings.FrameworkRestrictions)
        |> not)
    |> Set.union rest


type Resolved = {
    ResolvedPackages : Resolution
    ResolvedSourceFiles : ModuleResolver.ResolvedSourceFile list
}

let getResolverStrategy globalStrategyForDirectDependencies globalStrategyForTransitives (allRequirementsOfCurrentPackage:Set<PackageRequirement>) (currentRequirement:PackageRequirement) =
    if currentRequirement.Parent.IsRootRequirement() && Set.count allRequirementsOfCurrentPackage = 1 then
        let combined = currentRequirement.ResolverStrategyForDirectDependencies ++ globalStrategyForDirectDependencies

        defaultArg combined ResolverStrategy.Max
    else
        let combined =
            (allRequirementsOfCurrentPackage
                |> Seq.filter (fun x -> x.Depth > 0)
                |> Seq.sortBy (fun x -> x.Depth, x.ResolverStrategyForTransitives <> globalStrategyForTransitives, x.ResolverStrategyForTransitives <> Some ResolverStrategy.Max)
                |> Seq.map (fun x -> x.ResolverStrategyForTransitives)
                |> Seq.fold (++) None)
                ++ globalStrategyForTransitives

        defaultArg combined ResolverStrategy.Max

type UpdateMode =
    | UpdateGroup of GroupName
    | UpdateFiltered of GroupName * PackageFilter
    | Install
    | UpdateAll

type private PackageConfig = {
    Dependency         : PackageRequirement
    GroupName          : GroupName
    GlobalRestrictions : FrameworkRestrictions
    RootSettings       : IDictionary<PackageName,InstallSettings>
    Version            : SemVerInfo
    Sources            : PackageSource list
    UpdateMode         : UpdateMode
} with
    member self.HasGlobalRestrictions =
        not(getExplicitRestriction self.GlobalRestrictions = FrameworkRestriction.NoRestriction)

    member self.HasDependencyRestrictions =
        not(getExplicitRestriction self.Dependency.Settings.FrameworkRestrictions = FrameworkRestriction.NoRestriction)


let private updateRestrictions (pkgConfig:PackageConfig) (package:ResolvedPackage) =
    let newRestrictions =
        if  not pkgConfig.HasGlobalRestrictions
            && (FrameworkRestriction.NoRestriction = (package.Settings.FrameworkRestrictions |> getExplicitRestriction)
            ||  not pkgConfig.HasDependencyRestrictions )
        then
            FrameworkRestriction.NoRestriction
        else
            // Setting in the dependencies file
            let globalPackageSettings =
                match pkgConfig.RootSettings.TryGetValue package.Name with
                | true, s -> 
                    match s.FrameworkRestrictions with
                    | ExplicitRestriction r -> r
                    | _ -> FrameworkRestriction.NoRestriction
                | _ -> FrameworkRestriction.NoRestriction
            // Settings required for the current resolution
            let packageSettings = package.Settings.FrameworkRestrictions |> getExplicitRestriction
            // Settings required for this current dependency
            let dependencySettings = pkgConfig.Dependency.Settings.FrameworkRestrictions |> getExplicitRestriction
            // Settings defined globally
            let globalSettings = pkgConfig.GlobalRestrictions |> getExplicitRestriction
            let isRequired =
                FrameworkRestriction.Or
                  [ packageSettings
                    FrameworkRestriction.And [dependencySettings;globalSettings]]

            // We assume the user knows what he is doing
            FrameworkRestriction.And [ globalPackageSettings;isRequired ]


    { package with
        Settings = { package.Settings with FrameworkRestrictions = ExplicitRestriction newRestrictions }
    }


let private explorePackageConfig getPackageDetailsBlock (pkgConfig:PackageConfig) =
    let dependency, version = pkgConfig.Dependency, pkgConfig.Version
    let packageSources      = pkgConfig.Sources

    match pkgConfig.UpdateMode with
    | Install -> tracefn  " - %O %A" dependency.Name version
    | _ ->
        match dependency.VersionRequirement.Range with
        | Specific _ when dependency.Parent.IsRootRequirement() -> traceWarnfn " - %O is pinned to %O" dependency.Name version
        | _ ->
            let compareString = dependency.Name.CompareString
            tracefn  " - %O %A" dependency.Name version

    let newRestrictions =
        filterRestrictions dependency.Settings.FrameworkRestrictions pkgConfig.GlobalRestrictions
    try
        let packageDetails : PackageDetails =
            getPackageDetailsBlock packageSources pkgConfig.GroupName dependency.Name version
        let filteredDependencies =
            DependencySetFilter.filterByRestrictions newRestrictions packageDetails.DirectDependencies
        let settings =
            match dependency.Parent with
            | DependenciesFile _ -> dependency.Settings
            | Package _ ->
                match pkgConfig.RootSettings.TryGetValue packageDetails.Name with
                | true, s -> s + dependency.Settings
                | _ -> dependency.Settings
            |> fun x -> x.AdjustWithSpecialCases packageDetails.Name
        Some
            {   Name                = packageDetails.Name
                Version             = version
                Dependencies        = filteredDependencies
                Unlisted            = packageDetails.Unlisted
                Settings            = { settings with FrameworkRestrictions = newRestrictions }
                Source              = packageDetails.Source
                IsRuntimeDependency = false
            }
    with
    | exn ->
        traceWarnfn "    Package not available.%s      Message: %s" Environment.NewLine exn.Message
        None


type StackPack = {
    ExploredPackages     : Dictionary<PackageName*SemVerInfo,ResolvedPackage>
    KnownConflicts       : HashSet<HashSet<PackageRequirement> * ((SemVerInfo * PackageSource list) list * bool) option>
    ConflictHistory      : Dictionary<PackageName, int>
}


let private getExploredPackage (pkgConfig:PackageConfig) getPackageDetailsBlock (stackpack:StackPack) =
    let key = (pkgConfig.Dependency.Name, pkgConfig.Version)

    match stackpack.ExploredPackages.TryGetValue key with
    | true, package ->
        let package = updateRestrictions pkgConfig package
        stackpack.ExploredPackages.[key] <- package
        if verbose then
            verbosefn "   Retrieved explored package %O" package
        stackpack, Some(true, package)
    | false,_ ->
        match explorePackageConfig getPackageDetailsBlock pkgConfig with
        | Some explored ->
            if verbose then
                verbosefn "   Found explored package %O" explored
            stackpack.ExploredPackages.Add(key,explored)
            stackpack, Some(false, explored)
        | None ->
            stackpack, None



let private getCompatibleVersions
               (currentStep:ResolverStep)
                groupName
               (currentRequirement:PackageRequirement)
               (getVersionsF: PackageSource list -> ResolverStrategy -> GroupName -> PackageName -> seq<SemVerInfo * PackageSource list>)
                globalOverride
                globalStrategyForDirectDependencies
                globalStrategyForTransitives        =
    if verbose then
        verbosefn "  Trying to resolve %O" currentRequirement

    match Map.tryFind currentRequirement.Name currentStep.FilteredVersions with
    | None ->
        let allRequirementsOfCurrentPackage =
            currentStep.OpenRequirements
            |> Set.filter (fun r -> currentRequirement.Name = r.Name)

        // we didn't select a version yet so all versions are possible
        let isInRange mapF (ver,_) =
            allRequirementsOfCurrentPackage
            |> Set.forall (fun r -> (mapF r).VersionRequirement.IsInRange ver)

        let getSingleVersion v =
            match currentRequirement.Parent with
            | PackageRequirementSource.Package(_,_,parentSource) ->
                let sources = parentSource :: currentRequirement.Sources |> List.distinct
                Seq.singleton (v,sources)
            | _ ->
                let sources : PackageSource list = currentRequirement.Sources |> List.sortBy (fun x -> not x.IsLocalFeed, String.containsIgnoreCase "nuget.org" x.Url |> not)
                Seq.singleton (v,sources)

        let availableVersions =
            match currentRequirement.VersionRequirement.Range with
            | OverrideAll v -> getSingleVersion v
            | Specific v -> getSingleVersion v
            | _ ->
                let resolverStrategy = getResolverStrategy globalStrategyForDirectDependencies globalStrategyForTransitives allRequirementsOfCurrentPackage currentRequirement
                getVersionsF currentRequirement.Sources resolverStrategy groupName currentRequirement.Name

        let compatibleVersions = Seq.filter (isInRange id) (availableVersions)
        let compatibleVersions, globalOverride =
            if currentRequirement.VersionRequirement.Range.IsGlobalOverride then
                compatibleVersions |> Seq.cache, true
            elif Seq.isEmpty compatibleVersions then
                let prereleaseStatus (r:PackageRequirement) =
                    if r.Parent.IsRootRequirement() && r.VersionRequirement <> VersionRequirement.AllReleases then
                        r.VersionRequirement.PreReleases
                    else
                        PreReleaseStatus.All

                let available = availableVersions |> Seq.toList
                let prereleases = List.filter (isInRange (fun r -> r.IncludingPrereleases(prereleaseStatus r))) available
                let allPrereleases = prereleases |> List.filter (fun (v,_) -> v.PreRelease <> None) = prereleases
                if allPrereleases then
                    Seq.ofList prereleases, globalOverride
                else
                    compatibleVersions|> Seq.cache, globalOverride
            else
                compatibleVersions|> Seq.cache, globalOverride

        compatibleVersions, globalOverride, false

    | Some(versions,globalOverride) ->
        // we already selected a version so we can't pick a different
        let compatibleVersions, tryRelaxed =
            if globalOverride then List.toSeq versions, false else
            let compat =
                Seq.filter (fun (v,_) -> currentRequirement.VersionRequirement.IsInRange(v,currentRequirement.Parent.IsRootRequirement() |> not)) versions

            if Seq.isEmpty compat then
                let withPrereleases = Seq.filter (fun (v,_) -> currentRequirement.IncludingPrereleases().VersionRequirement.IsInRange(v,currentRequirement.Parent.IsRootRequirement() |> not)) versions
                if currentStep.Relax || Seq.isEmpty withPrereleases then
                    withPrereleases, false
                else
                    withPrereleases, true
            else
                compat, false
        compatibleVersions, false, tryRelaxed


let private getConflicts (currentStep:ResolverStep) (currentRequirement:PackageRequirement) (knownConflicts:HashSet<HashSet<PackageRequirement> * ((SemVerInfo * PackageSource list) list * bool) option>) =
    
    let allRequirements =
        currentStep.OpenRequirements
        |> Set.filter (fun r -> r.Graph |> Set.contains currentRequirement |> not)
        |> Set.union currentStep.ClosedRequirements

    knownConflicts
    |> Seq.map (fun (conflicts,selectedVersion) ->
        let isSubset = conflicts.IsSubsetOf allRequirements
        match selectedVersion with
        | None when isSubset -> conflicts
        | Some(selectedVersion,_) ->
            let n = (Seq.head conflicts).Name
            match currentStep.FilteredVersions |> Map.tryFind n with
            | Some(v,_) when v = selectedVersion && isSubset -> conflicts
            | _ -> HashSet()
        | _ -> HashSet())
    |> Seq.collect id
    |> HashSet


let private getCurrentRequirement packageFilter (openRequirements:Set<PackageRequirement>) (conflictHistory:Dictionary<_,_>) =
    let initialMin = Seq.head openRequirements
    let initialBoost = 0

    let currentMin, _ =
        ((initialMin,initialBoost),openRequirements)
        ||> Seq.fold (fun (cmin,cboost) d ->
            let boost =
                match conflictHistory.TryGetValue d.Name with
                | true,c -> -c
                | _ -> 0
            if PackageRequirement.Compare(d,cmin,packageFilter,boost,cboost) = -1 then
                d, boost
            else
                cmin,cboost)
    currentMin


[<StructuredFormatDisplay "{Display}">]
type ConflictState = {
    Status               : Resolution
    LastConflictReported : DateTime
    TryRelaxed           : bool
    Conflicts            : Set<PackageRequirement>
    VersionsToExplore    : seq<SemVerInfo * PackageSource list>
    GlobalOverride       : bool
} with
    member private self.Display 
        with get () = 
            let conflicts = self.Conflicts |> Seq.map (printfn "%A\n") |> String.Concat
            let explore = self.VersionsToExplore |> Seq.map (printfn "%A\n") |> String.Concat
            sprintf 
               "[< ConflictState >]\n\
                | Status       - %A\n\
                | Conflicts    - %A\n\
                | ExploreVers  - %A\n\
                | TryRelaxed   - %A\n
                | LastReport   - %A\n"                
                    self.Status conflicts explore 
                    self.TryRelaxed self.LastConflictReported
        
        
let private boostConflicts
                    (filteredVersions:Map<PackageName, ((SemVerInfo * PackageSource list) list * bool)>)
                    (currentRequirement:PackageRequirement)
                    (stackpack:StackPack)
                    (conflictState:ConflictState) =
    let conflictStatus = conflictState.Status
    let isNewConflict  =
        match stackpack.ConflictHistory.TryGetValue currentRequirement.Name with
        | true,count ->
            stackpack.ConflictHistory.[currentRequirement.Name] <- count + 1
            false
        | _ ->
            stackpack.ConflictHistory.Add(currentRequirement.Name, 1)
            true

    let conflicts = conflictStatus.GetConflicts()
    let lastConflictReported =
        match conflicts with
        | _ when not conflicts.IsEmpty  ->
            let c = conflicts |> Seq.minBy (fun c -> c.Parent)
            let selectedVersion = Map.tryFind c.Name filteredVersions
            let key = conflicts |> HashSet,selectedVersion
            stackpack.KnownConflicts.Add key |> ignore

            let reportThatResolverIsTakingLongerThanExpected =
                not isNewConflict && DateTime.Now - conflictState.LastConflictReported > TimeSpan.FromSeconds 10.

            if Logging.verbose then
                tracefn "%s" <| conflictStatus.GetErrorText(false)
                tracefn "    ==> Trying different resolution."
            if reportThatResolverIsTakingLongerThanExpected then
                traceWarnfn "%s" <| conflictStatus.GetErrorText(false)
                traceWarn "The process is taking longer than expected."
                traceWarn "Paket may still find a valid resolution, but this might take a while."
                DateTime.Now
            else
                conflictState.LastConflictReported
        | _ -> conflictState.LastConflictReported
    { conflictState with
        LastConflictReported = lastConflictReported }, stackpack


[<Struct>]
type private StepFlags = {
    Ready          : bool
    UseUnlisted    : bool
    HasUnlisted    : bool
    ForceBreak     : bool
    FirstTrial     : bool
    UnlistedSearch : bool
} with
    override self.ToString () =
        sprintf 
            "[< FLAGS >]\n\
            | Ready          - %b\n\
            | UseUnlisted    - %b\n\
            | HasUnlisted    - %b\n\
            | ForceBreak     - %b\n\
            | FirstTrial     - %b\n\   
            | UnlistedSearch - %b\n"   
            self.Ready self.UseUnlisted self.HasUnlisted self.ForceBreak self.FirstTrial self.UnlistedSearch

type private Stage =
    | Step  of currentConflict : (ConflictState * ResolverStep * PackageRequirement) * priorConflictSteps : (ConflictState * ResolverStep * PackageRequirement *  seq<SemVerInfo * PackageSource list> * StepFlags) list
    | Outer of currentConflict : (ConflictState * ResolverStep * PackageRequirement) * priorConflictSteps : (ConflictState * ResolverStep * PackageRequirement *  seq<SemVerInfo * PackageSource list> * StepFlags) list
    | Inner of currentConflict : (ConflictState * ResolverStep * PackageRequirement) * priorConflictSteps : (ConflictState * ResolverStep * PackageRequirement *  seq<SemVerInfo * PackageSource list> * StepFlags) list

/// Resolves all direct and transitive dependencies
let Resolve (getVersionsRaw, getPreferredVersionsRaw, getPackageDetailsRaw, groupName:GroupName, globalStrategyForDirectDependencies, globalStrategyForTransitives, globalFrameworkRestrictions, (rootDependencies:PackageRequirement Set), updateMode : UpdateMode) =
    tracefn "Resolving packages for group %O:" groupName
       
    use d = Profile.startCategory Profile.Category.ResolverAlgorithm

    let startedGetPackageDetailsRequests = System.Collections.Concurrent.ConcurrentDictionary<_,System.Threading.Tasks.Task<_>>()
    let startRequestGetPackageDetails sources groupName packageName semVer =
        let key = (sources, packageName, semVer)
        startedGetPackageDetailsRequests.GetOrAdd (key, fun _ ->
        (getPackageDetailsRaw sources groupName packageName semVer : Async<PackageDetails>)
            |> Async.StartAsTask)
    let getPackageDetailsBlock sources groupName packageName semVer =
        use d = Profile.startCategory (Profile.Category.ResolverAlgorithmBlocked Profile.BlockReason.PackageDetails)
        let result = (startRequestGetPackageDetails sources groupName packageName semVer).GetAwaiter().GetResult()
        d.Dispose()
        result

    
    let startedGetVersionsRequests = System.Collections.Concurrent.ConcurrentDictionary<_,System.Threading.Tasks.Task<_>>()
    let startRequestGetVersions sources groupName packageName =
        let key = (sources, packageName)
        startedGetVersionsRequests.GetOrAdd (key, fun _ ->
            getVersionsRaw sources groupName packageName
            |> Async.StartAsTask)
    let getVersionsBlock sources resolverStrategy groupName packageName =
        use d = Profile.startCategory (Profile.Category.ResolverAlgorithmBlocked Profile.BlockReason.GetVersion)
        let versions = (startRequestGetVersions sources groupName packageName).GetAwaiter().GetResult() |> Seq.toList
        d.Dispose()
        let sorted =
            match resolverStrategy with
            | ResolverStrategy.Max -> List.sortDescending versions
            | ResolverStrategy.Min -> List.sort versions
        let pref = getPreferredVersionsRaw sources resolverStrategy groupName packageName
        pref @ sorted |> List.toSeq

    let packageFilter =
        match updateMode with
        | UpdateFiltered (g, f) when g = groupName -> Some f
        | _ -> None

    let rootSettings =
        rootDependencies
        |> Seq.map (fun x -> x.Name,x.Settings)
        |> dict

    let lockedPackages =
        rootDependencies
        |> Seq.choose (fun d ->
            match d.VersionRequirement with
            | VersionRequirement.VersionRequirement(VersionRange.OverrideAll v, _) -> Some (d.Name)
            | _ -> None)
        |> Set.ofSeq


    if Set.isEmpty rootDependencies then Resolution.Ok Map.empty 
    else

    /// Evaluates whethere the innermost step-looping stage should continue or not
    let keepLooping (flags:StepFlags) (conflictState:ConflictState) =
        if flags.ForceBreak then false else
        if conflictState.Status.IsDone || Seq.isEmpty conflictState.VersionsToExplore then false else
        flags.FirstTrial || Set.isEmpty conflictState.Conflicts
        

    let rec step (stage:Stage) (stackpack:StackPack) compatibleVersions (flags:StepFlags) =

        let inline fuseConflicts currentConflict priorConflictSteps conflicts =
            let findMatchingStep priorConflictSteps =
                let currentNames =
                    conflicts
                    |> Seq.map (fun c ->
                        c.Graph
                        |> Set.map (fun (pr:PackageRequirement) -> pr.Name) 
                        |> Set.add c.Name)
                    |> Set.unionMany
                priorConflictSteps
                |> List.tryExtractOne (fun (_,_,lastRequirement:PackageRequirement,_,_) ->
                    currentNames |> Set.contains lastRequirement.Name)

            match findMatchingStep priorConflictSteps with
            | None, []  -> currentConflict
            | (Some head), priorConflictSteps ->
                let (lastConflict, lastStep, lastRequirement, lastCompatibleVersions, lastFlags) = head
                let continueConflict = 
                    { currentConflict with VersionsToExplore = lastConflict.VersionsToExplore }        
                step (Inner((continueConflict,lastStep,lastRequirement), priorConflictSteps))  stackpack lastCompatibleVersions lastFlags
            // could not find a specific package - go back one step
            | None, head :: priorConflictSteps ->
                let (lastConflict, lastStep, lastRequirement, lastCompatibleVersions, lastFlags) = head
                let continueConflict = 
                    { currentConflict with VersionsToExplore = lastConflict.VersionsToExplore }        
                step (Inner((continueConflict,lastStep,lastRequirement), priorConflictSteps))  stackpack lastCompatibleVersions lastFlags
                        
        match stage with            
        | Step((currentConflict,currentStep,_currentRequirement), priorConflictSteps)  -> 
            if Set.isEmpty currentStep.OpenRequirements then
                let currentConflict =
                    { currentConflict with
                        Status = Resolution.Ok (cleanupNames currentStep.CurrentResolution) }
              
                match currentConflict, priorConflictSteps with
                | currentConflict, (lastConflict,lastStep,lastRequirement,lastCompatibleVersions,lastFlags)::priorConflictSteps -> 
                    let continueConflict = {
                        currentConflict with
                            VersionsToExplore = lastConflict.VersionsToExplore
                    }        
                    match continueConflict.Status with
                    | Resolution.Conflict (_,conflicts,_,_)
                        when
                            (Set.isEmpty conflicts |> not)
                            && currentStep.CurrentResolution.Count > 1
                            && not (conflicts |> Set.exists (fun r ->
                                r = lastRequirement
                                || r.Graph |> Set.contains lastRequirement)) ->
                       
                        step (Inner((continueConflict,lastStep,lastRequirement),priorConflictSteps)) stackpack lastCompatibleVersions  { flags with ForceBreak = true } 
                    | _ ->
                        step (Inner((continueConflict,lastStep,lastRequirement),priorConflictSteps)) stackpack lastCompatibleVersions  lastFlags 
                
                | currentConflict, [] -> currentConflict

            else
                if Logging.verbose then
                    verbosefn "   %d packages in resolution.%s\n   %d requirements left%s\n" 
                        currentStep.CurrentResolution.Count 
                        (currentStep.CurrentResolution |> Seq.map (fun x -> sprintf "\n     - %O, %O" x.Key x.Value.Version) |> String.Concat)
                        currentStep.OpenRequirements.Count
                        (currentStep.OpenRequirements  |> Seq.map (fun x -> sprintf "\n     - %O, %O (from %O)" x.Name x.VersionRequirement x.Parent) |> String.Concat)

                let currentRequirement = 
                    getCurrentRequirement packageFilter currentStep.OpenRequirements stackpack.ConflictHistory

                let conflicts = 
                    getConflicts currentStep currentRequirement stackpack.KnownConflicts

                let currentConflict =
                    let getVersionsF =
                        getVersionsBlock currentRequirement.Sources ResolverStrategy.Max groupName
                    if Seq.isEmpty conflicts then
                        { currentConflict with
                            Status = Resolution.Conflict (currentStep,Set.empty,currentRequirement,getVersionsF)}
                    else
                        { currentConflict with
                            Status = Resolution.Conflict (currentStep,set conflicts,Seq.head conflicts,getVersionsF)}

                if not (Seq.isEmpty conflicts) then                
                    fuseConflicts currentConflict priorConflictSteps conflicts
                else
                    let compatibleVersions,globalOverride,tryRelaxed =
                        getCompatibleVersions currentStep groupName currentRequirement getVersionsBlock
                                currentConflict.GlobalOverride
                                globalStrategyForDirectDependencies
                                globalStrategyForTransitives

                    let currentConflict = {
                        currentConflict with
                            Conflicts           = set conflicts
                            TryRelaxed          = tryRelaxed
                            GlobalOverride      = globalOverride
                    }
                    let conflictState, stackpack =
                        if Seq.isEmpty compatibleVersions then
                            boostConflicts currentStep.FilteredVersions currentRequirement stackpack currentConflict
                        else
                            currentConflict, stackpack
                    let flags = { 
                      flags with
                        Ready       = false
                        UseUnlisted = false
                        HasUnlisted = false
                        UnlistedSearch = false
                    }
                    step (Outer ((conflictState,currentStep,currentRequirement),priorConflictSteps)) stackpack compatibleVersions  flags 
        | Outer ((currentConflict,currentStep,currentRequirement), priorConflictSteps) ->
            if flags.Ready then
                fuseConflicts currentConflict priorConflictSteps (HashSet [ currentRequirement ])
            else
                let flags = {
                  flags with
                    ForceBreak = false 
                    FirstTrial = true
                }
                let currentConflict = { currentConflict with VersionsToExplore = compatibleVersions }
                step (Inner ((currentConflict,currentStep,currentRequirement), priorConflictSteps)) stackpack compatibleVersions  flags 

        | Inner ((currentConflict,currentStep,currentRequirement), priorConflictSteps)->
            if not (keepLooping flags currentConflict) then
                let flags =
                    if  not flags.UseUnlisted 
                     && flags.HasUnlisted 
                     && not flags.UnlistedSearch
                     && not currentConflict.Status.IsDone 
                     then
                     // if it's been determined that an unlisted package must be used, ready must be set to false
                        if verbose then
                            verbosefn "\nSearching for compatible unlisted package\n"
                        { flags with 
                            Ready = false
                            UseUnlisted = true
                            UnlistedSearch = true
                        }
                    else
                        { flags with 
                            Ready = true 
                            UnlistedSearch = true 
                        }
                step (Outer((currentConflict,currentStep,currentRequirement), priorConflictSteps)) stackpack compatibleVersions  flags 
            else
                

                let flags = { flags with FirstTrial = false }
                let (version,sources) & versionToExplore = Seq.head currentConflict.VersionsToExplore

                let currentConflict = 
                    { currentConflict with VersionsToExplore = Seq.tail currentConflict.VersionsToExplore }

                let packageDetails = {
                    GroupName          = groupName
                    Dependency         = currentRequirement
                    GlobalRestrictions = globalFrameworkRestrictions
                    RootSettings       = rootSettings
                    Version            = version
                    Sources            = sources
                    UpdateMode         = updateMode
                }

                match getExploredPackage packageDetails getPackageDetailsBlock stackpack with
                | stackpack, None ->
                    step (Inner((currentConflict,currentStep,currentRequirement), priorConflictSteps)) stackpack compatibleVersions  flags

                | stackpack, Some(alreadyExplored,exploredPackage) ->
                    let hasUnlisted = exploredPackage.Unlisted || flags.HasUnlisted
                    let flags = { flags with HasUnlisted = hasUnlisted }

                    // Start pre-loading infos about dependencies.
                    for (pack,verReq,restr) in exploredPackage.Dependencies do
                        async {
                            let! versions = startRequestGetVersions currentRequirement.Sources groupName pack |> Async.AwaitTask
                            // Preload the first version in range of this requirement
                            match versions |> Seq.map fst |> Seq.tryFind (verReq.IsInRange) with
                            | Some verToPreload ->
                                let! details = startRequestGetPackageDetails currentRequirement.Sources groupName pack verToPreload |> Async.AwaitTask
                                ()
                            | None -> ()
                            return ()
                        } |> Async.Start

                    if exploredPackage.Unlisted && not flags.UseUnlisted then
                        if not alreadyExplored then
                            tracefn "     %O %O was unlisted" exploredPackage.Name exploredPackage.Version
                        step (Inner ((currentConflict,currentStep,currentRequirement), priorConflictSteps)) stackpack compatibleVersions flags 
                    else
                        // It might be that this version is already not possible because of our current set.
                        // Example: We took A with version 1.0.0 (in our current resolution), but this version depends on A > 1.0.0
                        let packageErrors =
                            currentStep.CurrentResolution
                            |> Map.toSeq
                            |> Seq.map snd
                            // Ignore packages which have "OverrideAll", otherwise == will not work anymore.
                            |> Seq.filter (fun resolved -> lockedPackages.Contains resolved.Name |> not)
                            |> Seq.choose (fun r ->
                                let incompatible = DependencySetFilter.findIncompatibleDependencies exploredPackage.Dependencies r
                                if Seq.isEmpty incompatible  then
                                    None
                                else
                                    Some incompatible)
                            |> Seq.toList
                        
                        match packageErrors with
                        | [] ->
                            let nextStep =
                                {   Relax              = currentStep.Relax
                                    FilteredVersions   = Map.add currentRequirement.Name ([versionToExplore],currentConflict.GlobalOverride) currentStep.FilteredVersions
                                    CurrentResolution  = Map.add exploredPackage.Name exploredPackage currentStep.CurrentResolution
                                    ClosedRequirements = Set.add currentRequirement currentStep.ClosedRequirements
                                    OpenRequirements   = calcOpenRequirements(exploredPackage,globalFrameworkRestrictions,versionToExplore,currentRequirement,currentStep)
                                }
                            if nextStep.OpenRequirements = currentStep.OpenRequirements then
                                failwithf "The resolver confused itself. The new open requirements are the same as the old ones.\n\
                                           This will result in an endless loop.%sCurrent Requirement: %A%sRequirements: %A"
                                                Environment.NewLine currentRequirement Environment.NewLine nextStep.OpenRequirements
                            step (Step((currentConflict,nextStep,currentRequirement), (currentConflict,currentStep,currentRequirement,compatibleVersions,flags)::priorConflictSteps)) stackpack currentConflict.VersionsToExplore flags
                        | firstError :: _ ->
                            match currentConflict.Status with
                            | Resolution.Ok _ -> 
                                step (Inner ((currentConflict,currentStep,currentRequirement), priorConflictSteps)) stackpack compatibleVersions flags
                            | Resolution.Conflict(resolveStep,requirementSet,requirement,getPackageVersions) ->
                                let requirements =
                                    firstError
                                    |> Seq.map (fun (name,vr,_) -> 
                                            { currentRequirement with
                                                  Name = name
                                                  VersionRequirement = vr
                                                  Parent = PackageRequirementSource.Package(currentRequirement.Name, exploredPackage.Version, exploredPackage.Source)
                                                  Graph = Set.add currentRequirement currentRequirement.Graph })
                                    |> Seq.fold (fun requirements r -> Set.add r requirements) requirementSet
                                let currentConflict = 
                                    { currentConflict with
                                        Status = Resolution.Conflict(resolveStep,requirements,requirement,getPackageVersions) }
                                
                                step (Inner ((currentConflict,currentStep,currentRequirement), priorConflictSteps)) stackpack compatibleVersions flags

    let startingStep = {
        Relax              = false
        FilteredVersions   = Map.empty
        CurrentResolution  = Map.empty
        ClosedRequirements = Set.empty
        OpenRequirements   = rootDependencies
    }

    for openReq in startingStep.OpenRequirements do
        startRequestGetVersions openReq.Sources groupName openReq.Name
        |> ignore

    let currentRequirement = getCurrentRequirement packageFilter startingStep.OpenRequirements (Dictionary())

    let status =
        let getVersionsF = getVersionsBlock currentRequirement.Sources ResolverStrategy.Max groupName
        Resolution.Conflict(startingStep,Set.empty,currentRequirement,getVersionsF)


    let currentConflict : ConflictState = {
        Status               = (status : Resolution)
        LastConflictReported = DateTime.Now
        TryRelaxed           = false
        GlobalOverride       = false
        Conflicts            = (Set.empty : Set<PackageRequirement>)
        VersionsToExplore    = (Seq.empty : seq<SemVerInfo * PackageSource list>)
    }

    let stackpack = {
        ExploredPackages     = Dictionary<PackageName*SemVerInfo,ResolvedPackage>()
        KnownConflicts       = (HashSet() : HashSet<HashSet<PackageRequirement> * ((SemVerInfo * PackageSource list) list * bool) option>)
        ConflictHistory      = (Dictionary() : Dictionary<PackageName, int>)
    }

    let flags = {
        Ready       = false
        UseUnlisted = false
        HasUnlisted = false
        ForceBreak  = false
        FirstTrial  = true
        UnlistedSearch = false
    }

    let inline calculate () = step (Step((currentConflict,startingStep,currentRequirement),[])) stackpack Seq.empty flags
#if DEBUG
    let mutable results = None
    let mutable error = None
    // Increase stack size, because we have no tail-call-elimination
    let thread = new System.Threading.Thread((fun () ->
        try
            results <- Some (calculate())
        with e ->
            // Prevent the application from crashing
            error <- Some (System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture e)
        ), 1024 * 1024 * 100)
    thread.Name <- sprintf "Paket Resolver Thread (Debug) - %O" (System.Guid.NewGuid())
    thread.Start()
    thread.Join()
    match error with
    | Some e -> e.Throw()
    | _ -> ()
    let stepResult =
        match results with
        | Some s -> s
        | None -> failwithf "Expected to get results from the resolver thread :/."
#else
    let stepResult = calculate()
#endif
    
    match stepResult  with
    | { Status = Resolution.Conflict _ } as conflict ->
        if conflict.TryRelaxed then
            stackpack.KnownConflicts.Clear()
            stackpack.ConflictHistory.Clear()
            (step (Step((conflict
                        ,{startingStep with Relax=true}
                        ,currentRequirement),[])) 
                  stackpack Seq.empty flags).Status
        else
            conflict.Status
    | x -> x.Status

