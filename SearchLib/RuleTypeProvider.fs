namespace SearchLib.RuleTypeProvider

open System.Reflection
open System.IO
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Text.RegularExpressions
open System.Linq

// Dummy
type RulesFile() =
    member __.Rules = ["a"]

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
        
        let rowTy = ProvidedTypeDefinition("Row", Some(typeof<string[]>))

        // add a parameterless constructor which loads the file that was used to define the schema
        ty.AddMember(ProvidedConstructor([], InvokeCode = fun [] -> <@@ RulesFile() @@>))
        
        for i in 0..(lines.Length - 1) do
            let l = lines.[i]
            let name, rest = 
                let i = l.IndexOf('(')
                if i < 0 then failwith("there is no ( in line " + l)
                else l.Substring(0, i), l.Substring(i, l.Length - i)

            let prop = ProvidedProperty(name, typeof<string>, GetterCode = fun [] -> <@@ name @@>)
            prop.AddDefinitionLocation(1, i, filename)
            ty.AddMember prop
            
        // add a new, more strongly typed Data property (which uses the existing property at runtime)
        ty.AddMember(ProvidedProperty("Data", typedefof<seq<_>>.MakeGenericType(rowTy), GetterCode = fun [file] -> <@@ (%%file:RulesFile).Rules @@>))
        
        ty.AddMember(ProvidedProperty("RRRow", typedefof<seq<_>>.MakeGenericType(rowTy), GetterCode = fun [file] -> <@@ (%%file:RulesFile).Rules @@>))

        ty.AddMember rowTy
        ty)

    // add the type to the namespace
    do this.AddNamespace(ns, [ruleTy])

[<TypeProviderAssembly>]
do()