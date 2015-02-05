namespace Frolog

open System.Reflection
open System.IO
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Text.RegularExpressions
open System.Linq

open DefineRule

(*This module is in development
module RuleTypeProvideModule =
    let extractRules (lines: string array) =
                let cnt = Array.length lines
                let parseRule (line: string): option<Rule> = 
                    let (|Regex|_|) pattern input =
                        let m = Regex.Match(input, pattern)
                        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
                        else None
                    match line with
                    | Regex "([a-z0-9]+)\(([a-z0-9]*)\)\." [name; parameters] -> 
                        sign (Structure(name, [])) |> Option.bind (fun f -> Some(defFact f))
                    | _ -> None
                lines |> Array.map parseRule |> Array.choose(fun s -> s) |> Array.toList

    type RulesFile(file: string) =
        new() = RulesFile("")
        member __.Rules : Rule list = 
            if not (File.Exists file) then []
            else File.ReadAllLines file |> Seq.filter(fun s -> not (s.Length = 0)) |> Seq.toArray |> extractRules

    type MyRule = {name: string}

    [<TypeProvider>]
    type public RulesTypeProvider(cfg:TypeProviderConfig) as this =
        inherit TypeProviderForNamespaces()

        // Get the assembly and namespace used to house the provided types
        let asm = System.Reflection.Assembly.GetExecutingAssembly()
        let ns = "SearchLib.RuleTypeProvider"

        // Create the main provided type
        let ruleTy = ProvidedTypeDefinition(asm, ns, "CustomRules", Some(typeof<obj>))

        // Parameterize the type by the file to use as a template
        let filename = ProvidedStaticParameter("filename", typeof<string>)
        do ruleTy.DefineStaticParameters([filename], fun tyName [| :? string as filename |] ->

            // resolve the filename relative to the resolution folder
            let resolvedFilename = Path.Combine(cfg.ResolutionFolder, filename)
        
            // All rules now are oneliners
            // and rule name consist of one character
            let lines = File.ReadAllLines(resolvedFilename) |> Seq.filter(fun s -> not (s.Length = 0)) |> Seq.toArray

            // define the provided type, erasing to CsvFile
            let ty = ProvidedTypeDefinition(asm, ns, tyName, Some(typeof<RulesFile>))
        
            let rowTy = ProvidedTypeDefinition("Row", Some(typeof<Rule>))
        
            let rules = extractRules lines

            for i in 0..(rules.Length-1) do
                let r = rules.[i]
                let prop = ProvidedProperty(Signature.GetName r.Signature, typeof<Rule>, GetterCode = fun [] -> <@@ r @@>)
                prop.AddDefinitionLocation(i, 1, filename) // 1 or 0
                ty.AddMember prop
            
            // add a parameterless constructor which loads the file that was used to define the schema
            ty.AddMember(ProvidedConstructor([], InvokeCode = fun [] -> <@@ RulesFile(resolvedFilename) @@>))
        
            // add a new, more strongly typed Data property (which uses the existing property at runtime)
            // ty.AddMember(ProvidedProperty("Data", typedefof<seq<_>>.MakeGenericType(rowTy), GetterCode = fun [file] -> <@@ (%%file:RulesFile).Rules @@>))
        
            ty.AddMember rowTy
            ty)

        // add the type to the namespace
        do this.AddNamespace(ns, [ruleTy])

    #if RELEASE
    [<TypeProviderAssembly>]
    do()
    #endif
*)