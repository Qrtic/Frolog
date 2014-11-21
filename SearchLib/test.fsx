#r @".\bin\Debug\SearchLib.dll"

let rules = new SearchLib.RuleTypeProvider.CustomRules<"custom_rules.txt">()
// Here you can see your rules, defined in file
// let a = rules.aaa