<Query Kind="FSharpProgram" />

type 'a tree =
    | Nil
    | Leaf of 'a
    | Node of 'a tree * 'a tree
    
let rec prettyPrintHelper head tail =
    match head with
    | Nil -> printf "()" // Assume anything with a head of Nil is an empty list.
    | Leaf(value) ->
        match tail with
        | Nil -> printf "%A" value
        | Leaf(value2) -> printf "%A, %A" value value2
        | Node(h, t) ->
            printf "%A, " value
            prettyPrintHelper h t
    | Node(h, t) ->
        match tail with
        | Nil ->
            printf "("
            prettyPrintHelper h t
            printf ")"
        | Leaf(value) ->
            printf "("
            prettyPrintHelper h t
            printf "), %A" value
        | Node(h2, t2) ->
            printf "("
            prettyPrintHelper h t
            printf "), "
            prettyPrintHelper h2 t2
            printf ""

let prettyPrint (list : 'a tree) =
    printf "("
    match list with
    | Nil -> ()
    | Leaf(value) -> printf "%A" value
    | Node(head, tail) -> prettyPrintHelper head tail
    printfn ")"
    
let cons head tail =
    Node(head, tail)
    
let snoc tail head =
    Node(head, tail)
    
let rec reverse'(head: 'a tree) (tail: 'a tree) (acc: 'a tree): 'a tree =
    match acc with
    | Nil ->
    // Accumulator is empty / has not accumulated anything yet.
        match (head, tail) with
        // return node flipped
        | (Nil, Nil) -> cons head tail // Empty list: `()`
        | (Nil, Leaf(v)) -> cons tail head // Terminal Leaf, reversed: should be `value` but is technically `, value`.
        | (Nil, Node(h,t)) -> reverse' h t Nil |> snoc Nil // Terminal Node, reversed: should be `h, t...` but is technically `, h, t...`.
        | (Leaf(v), Nil) -> cons head tail // Terminal Leaf: `value`
        | (Leaf(v), Leaf(v')) -> cons tail head // Terminal Leaf pair: `value1, value2`
        | (Leaf(v), Node(h,t)) -> reverse' h t head // Standard node: `head-value, tail`
        | (Node(h,t), Nil) -> reverse' h t Nil |> snoc Nil // Terminal Node: `head-node`
        | (Node(h,t), Leaf(v)) -> reverse' h t Nil |> snoc Nil |> cons tail // Terminal Node-Leaf pair: `node, leaf`
        | (Node(h1,t1), Node(h2,t2)) -> reverse' h1 t1 Nil |> snoc Nil |> reverse' h2 t2 // Tree branching: `node, node`
    | _ ->
        // Accumulator not empty.
        match (head, tail) with
        | (Nil, Nil) -> cons head tail |> snoc acc // Empty list: `()`
        | (Nil, Leaf(v)) -> cons tail head |> snoc acc // Terminal Leaf, reversed: should be `value` but is technically `, value`.
        | (Nil, Node(h,t)) -> reverse' h t Nil |> snoc acc // Terminal Node, reversed: should be `h, t...` but is technically `, h, t...`.
        | (Leaf(v), Nil) -> cons head acc // Terminal Leaf: `value`
        | (Leaf(v), Leaf(v')) -> cons head acc |> cons tail // Terminal Leaf pair: `value1, value2`
        | (Leaf(v), Node(h,t)) -> cons head acc |> reverse' h t // Standard node: `head-value, tail`
        | (Node(h,t), Nil) -> reverse' h t Nil |> snoc acc // Terminal Node: `head-node`
        | (Node(h,t), Leaf(v)) -> reverse' h t Nil |> snoc acc |> cons tail // Terminal Node-Leaf pair: `node, leaf`
        | (Node(h1,t1), Node(h2,t2)) -> reverse' h1 t1 Nil |> snoc acc |> reverse' h2 t2 // Tree branching: `node, node`
    
let rec reverse (list: 'a tree) : 'a tree = 
    match list with
    | Nil -> Nil
    | Leaf(value) -> list
    | Node(head, tail) ->
        match (head, tail) with
        | (Nil, Nil) -> list
        | (Nil, Leaf(v)) -> Node(tail, head) // Normalize
        | (Nil, Node(h, t)) -> reverse' tail head Nil // Normalize and reverse
        | (Leaf(v), Nil) -> list
        | (Leaf(v), Leaf(v')) -> Node(tail, head)
        | (Leaf(v), Node(h, t)) -> reverse' head tail Nil
        | (Node(h, t), _) -> reverse' head tail Nil
    
let input =
    Node(
        Leaf 1,
        Node(
            Node(
                Leaf 10,
                Node(
                    Leaf 20,
                    Node(
                        Node(
                            Leaf 100,
                            Leaf 200
                        ),
                        Leaf 30
                    )
                )
            ),
            Node(
                Leaf 2,
                Node(
                    Leaf 3,
                    Node(
                        Node(
                            Leaf 40,
                            Node(
                                Node( //100
                                    Node( //1,000
                                        Node( //10,000
                                            Node( //100,000
                                                Node( //1,000,000
                                                    Leaf 1_000_000,
                                                    Node(
                                                        Leaf 1_000_001,
                                                        Node(
                                                            Leaf 1_000_010,
                                                            Node(
                                                                Leaf 1_000_011,
                                                                Leaf 1_000_100
                                                            )
                                                        )
                                                    )
                                                ),
                                                Nil
                                            ),
                                            Nil
                                        ),
                                        Nil
                                    ),
                                    Nil
                                ),
                                Leaf 50
                            )
                        ),
                        Leaf 5
                    )
                )
            )
        )
    )
    
prettyPrint input
prettyPrint (reverse input)