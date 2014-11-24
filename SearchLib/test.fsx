#r @".\bin\Debug\SearchLib.dll"

let rules = new SearchLib.RuleTypeProvider.CustomRules<"custom_rules.txt">()
// Here you can see your rules, defined in file
printfn "%d rules" rules.Rules.Length
for r in rules.Rules do
    printfn "%s" r.Signature.ToStr