namespace RubiksCube.Core

module AStar =
    open System
    open System.Collections.Immutable

    [<CustomEquality; NoComparison>]
    type private Node<'Node,'Edge when 'Node : equality> =
        | StartNode of 'Node
        | Node of 'Node * 'Edge * float * Node<'Node,'Edge> with
        member this.Value =
            match this with
            | StartNode node -> node
            | Node (n,_,_,_) -> n
        member this.Cost =
            match this with
            | StartNode _ -> 0.0
            | Node (_,_,d,_) -> d
        override this.GetHashCode(): int = this.Value.GetHashCode()
        override this.Equals (other: obj) =
            match other with
            | :? Node<'Node,'Edge> as otherNode -> otherNode.Value.Equals this.Value
            | :? 'Node as otherNode -> otherNode.Equals this.Value
            | _ -> false

    type private OpenNode<'Node,'Edge when 'Node : equality>(g: 'Node -> float, node: Node<'Node,'Edge>) =
        let cost = node.Cost + g node.Value
        member __.Cost = cost
        member __.Node = node
        interface IComparable<OpenNode<'Node,'Edge>> with
            member __.CompareTo other = cost.CompareTo other.Cost

    type Problem<'Node,'Edge> =
        {
            startNode : 'Node
            edges: 'Node -> ('Node * ('Edge * float)) seq
            heuristics: 'Node -> float
            endCondition: 'Node -> bool
        }

    let rec private loop (problem: Problem<'Node,'Edge>)
                         (visited: Node<'Node,'Edge> ImmutableHashSet)
                         (openNodes: OpenNode<'Node,'Edge> ImmutableSortedSet): Node<'Node,'Edge> option =
        match openNodes.IsEmpty with
        | true -> None
        | false ->
            let n = openNodes.Min
            match problem.endCondition n.Node.Value with
            | true -> Some n.Node
            | false ->
                let openNodes = openNodes.Remove n
                n.Node.Value
                |> problem.edges
                |> Seq.map (fun (node,(edge,cost)) ->
                    Node (node, edge, cost, n.Node)
                )
                |> Seq.fold (fun ((visited: Node<'Node,'Edge> ImmutableHashSet),openNodes: OpenNode<'Node,'Edge> ImmutableSortedSet) newNode ->
                    let insertNew =
                        match visited.TryGetValue newNode with
                        | false, _ -> true
                        | true, (oldNode : Node<'Node,'Edge>) -> oldNode.Cost > newNode.Cost
                    if insertNew then
                        visited.Add newNode, openNodes.Add (OpenNode(problem.heuristics,newNode))
                    else
                        visited, openNodes
                ) (visited,openNodes)
                |> fun (visited,openNodes) ->
                    loop problem visited openNodes

    let private presentSolution (finalNode: Node<'Node,'Edge>): 'Node * ('Edge * 'Node) list * 'Node =
        let rec whatDoIEvenNameThis (finalNode: Node<'Node,'Edge>) (state: (('Edge * 'Node) list * 'Node) option): 'Node * ('Edge * 'Node) list * 'Node =
            match finalNode with
            | StartNode node ->
                match state with
                | None -> node, [], node
                | Some (steps,finish) -> node, steps, finish
            | Node (node,edge,_,parent) ->
                let state' =
                    match state with
                    | None ->
                        Some ([edge,node],node)
                    | Some (prev,final) ->
                        Some ((edge,node)::prev,final)
                whatDoIEvenNameThis parent state'
        whatDoIEvenNameThis finalNode None

    let solve (problem: Problem<'Node,'Edge>): ('Node * ('Edge * 'Node) list * 'Node) option =
        loop problem
             (ImmutableHashSet.Create (item = StartNode problem.startNode))
             (ImmutableSortedSet.Create (
                item = OpenNode<'Node,'Edge>(problem.heuristics, StartNode problem.startNode),
                comparer =
                    { new Collections.Generic.IComparer<OpenNode<'Node,'Edge>> with
                        member __.Compare (x, y) =
                            (x :> IComparable<OpenNode<'Node,'Edge>>).CompareTo y
                    }
             ))
        |> Option.map presentSolution

