namespace Frolog.Tests

open Frolog

// Test term create

open NUnit.Framework
open Frolog.Common

[<TestFixture>]
module MachineTests =
    let testMachineCache (machine: #SearchMachines.Custom) =
        machine.AddRule(DefineRule.DefPublic.defFact(signf "s(1)"))
        Assert.AreEqual(machine.CacheHits, 0)
        machine.Execute(signf "s(1)") |> ignore
        Assert.AreEqual(machine.CacheHits, 0)
        machine.Execute(signf "s(1)") |> ignore
        Assert.AreEqual(machine.CacheHits, 1)
    
    [<Test>]
    let testFifoCache() =
        let machine = Frolog.SearchMachines.Custom.CacheFirstMachine({maxPrecedences = 1})
        testMachineCache machine
        
    [<Test>]
    let testLifoCache() =
        let machine = Frolog.SearchMachines.Custom.CacheLastMachine({maxPrecedences = 1})
        testMachineCache machine