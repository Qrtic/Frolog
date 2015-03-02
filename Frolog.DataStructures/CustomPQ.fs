namespace Frolog.DataStructures

open System
open System.Linq

type CustomPQ<'a>(size: int, comparer) =
    let data: ResizeArray<'a> = new ResizeArray<'a>(size)
    member q.TryFind a =
        let res = data.FirstOrDefault(fun v -> comparer v a)
        match box (res) with
        | null -> None
        | _ -> Some res
    member q.Append a =
        let append() =
            if data.Count <= size then
                data.Add a
            else
                data.RemoveAt(0)
                data.Add a
        let i = data.FindIndex(fun v -> comparer v a)
        if i >= 0 then 
            let cur = data.[i]
            data.RemoveAt(i)
            append()
        else
            append()