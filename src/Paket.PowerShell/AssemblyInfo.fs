﻿namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Paket.PowerShell")>]
[<assembly: AssemblyProductAttribute("Paket")>]
[<assembly: AssemblyCompanyAttribute("Paket team")>]
[<assembly: AssemblyDescriptionAttribute("A package dependency manager for .NET with support for NuGet packages and GitHub repositories.")>]
[<assembly: AssemblyVersionAttribute("2.58.16")>]
[<assembly: AssemblyFileVersionAttribute("2.58.16")>]
[<assembly: AssemblyInformationalVersionAttribute("2.58.16")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.58.16"
    let [<Literal>] InformationalVersion = "2.58.16"
