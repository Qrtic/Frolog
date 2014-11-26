namespace SearchLib

module Common =
    type name = string
    type value = string
    
    let debug s = System.Diagnostics.Debug.WriteLine s
    let allways(a: 'a) = true