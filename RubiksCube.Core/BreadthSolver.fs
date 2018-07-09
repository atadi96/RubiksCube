namespace RubiksCube.Core

module BreadthSolver =
    open RubiksCube.Core.Utils
    open System.Collections.Immutable

    type BreadthNode<'Node,'Edge> =
        | StartNode of 'Node
        | Node of 'Node * 'Edge * int * BreadthNode<'Node,'Edge> with
        member this.GetNode =
            match this with
            | StartNode node -> node
            | Node (n,_,_,_) -> n
        member this.Depth =
            match this with
            | StartNode _ -> 0
            | Node (_,_,d,_) -> d

    type private BreadthState<'Node,'Edge> =
        {
            grey: 'Node ImmutableHashSet
            visited: BreadthNode<'Node,'Edge> ImmutableHashSet
            queue: BreadthNode<'Node,'Edge> ImmutableQueue
        }

    module private BreadthState =
        let pop (state: BreadthState<'A,'B>): BreadthNode<'A,'B> * BreadthState<'A,'B> =
            let peek = state.queue.Peek()
            peek
            ,
            { state with
                visited = state.visited.Add peek
                queue = state.queue.Dequeue()
            }

        let finished (maxDepth: int) (state: BreadthState<'A,'B>): bool =
            state.queue.IsEmpty ||
            match state.queue.Peek() with
            | Node (_,_,depth,_) -> depth > maxDepth
            | StartNode _ -> maxDepth < 1

        let private pushNode (bnode: BreadthNode<'A,'B>) (state: BreadthState<'A,'B>): BreadthState<'A,'B> =
            let node =
                match bnode with
                | StartNode n -> n
                | Node (n,_,_,_) -> n
            if state.grey.Contains node then
                state
            else
                { state with
                    grey = state.grey.Add node
                    queue = state.queue.Enqueue bnode
                }

        let push (nodes: BreadthNode<'A,'B> seq) (state: BreadthState<'A,'B>): BreadthState<'A,'B> =
            nodes
            |> Seq.fold (flip pushNode) state

        let peek (state: BreadthState<'A,'B>): BreadthNode<'A,'B> =
            state.queue.Peek()

    let private edgesOf (edges: 'Node -> #seq<'Node * 'Edge>) (parent: BreadthNode<'Node,'Edge>) =
        match parent with
        | StartNode node ->
            node
            |> edges
            |> Seq.map (fun (node, edge) -> Node (node, edge, 1, parent))
        | Node (node,_,depth,_) ->
            node
            |> edges
            |> Seq.map (fun (node, edge) -> Node (node, edge, depth + 1, parent))

    let private step
        (edges: 'Node -> #seq<'Node * 'Edge>)
        (state: BreadthState<'Node,'Edge>)
        : BreadthState<'Node,'Edge> =
            let (node,state') =
                state
                |> BreadthState.pop
            node
            |> edgesOf edges
            |> BreadthState.push <| state'

    let rec private solve
        (maxDepth: int)
        (edges: 'Node -> #seq<'Node * 'Edge>)
        (endCondition: 'Node -> bool)
        (userState: 'A)
        (stepUserState: 'A -> BreadthNode<'Node,'Edge> -> 'A)
        (state: BreadthState<'Node,'Edge>)
        : BreadthNode<'Node,'Edge> option =
            match state |> BreadthState.finished maxDepth with
            | true -> None
            | false ->
                let bnode = state |> BreadthState.peek
                let node = bnode.GetNode
                match node |> endCondition with
                | true -> Some bnode
                | false ->
                    state
                    |> step edges
                    |> solve maxDepth edges endCondition (stepUserState userState bnode) stepUserState

    let private presentSolution (finalNode: BreadthNode<'Node,'Edge>): 'Node * ('Edge * 'Node) list * 'Node =
        let rec whatDoIEvenNameThis (finalNode: BreadthNode<'Node,'Edge>) (state: (('Edge * 'Node) list * 'Node) option): 'Node * ('Edge * 'Node) list * 'Node =
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

    let search (maxDepth: int)
               (startNode: 'Node)
               (edges: 'Node -> #seq<'Node * 'Edge>)
               (endCondition: 'Node -> bool)
               (userState: 'A)
               (stepUserState: 'A -> BreadthNode<'Node,'Edge> -> 'A)
               : ('Node * ('Edge * 'Node) list * 'Node) option =
        match endCondition startNode with
        | true -> Some (startNode, [], startNode)
        | false ->
            let startBreadthNode = StartNode startNode
            let startState =
                {
                    grey = ImmutableHashSet.Create (item = startNode)
                    visited = ImmutableHashSet.Empty
                    queue = ImmutableQueue.Create startBreadthNode
                } : BreadthState<'Node,'Edge>
            startState
            |> solve maxDepth edges endCondition userState stepUserState
            |> Option.map presentSolution

