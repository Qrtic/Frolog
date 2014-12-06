namespace SearchLib

module Common =
    type name = string
    type value = string
    
    let debug s = System.Diagnostics.Debug.WriteLine s
    let allways(a: 'a) = true
    let equalsOpt<'T when 'T :> System.IEquatable<'T>> (x : 'T) (y : 'T) =
        x.Equals(y)

    let compareOpt (x : #System.IComparable<'T>) (y : #System.IComparable<'T>) = x.CompareTo(y)