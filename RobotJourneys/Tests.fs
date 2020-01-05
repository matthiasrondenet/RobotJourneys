module Tests

open System
open Xunit

type Position = int * int
type Direction = EAST | WEST | NORTH | SOUTH
type Command = TurnLeft | TurnRight | MoveForward
type RobotState = Position * Direction

type Input = {
    InitialRobotState : RobotState
    Commands : Command seq
    FinalRobotState : RobotState
}

let CreateInput (input:string) =
    let lines = input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries) |> Seq.map (fun s-> s.Trim())

    Console.WriteLine(sprintf "%A" lines)

    let mapDirection (c) =
        match c with
        | 'E' -> EAST
        | 'W' -> WEST
        | 'N' -> NORTH
        | 'S' -> SOUTH
        | a -> failwithf "Unkown direction %c" a

    let mapCommand (c) =
        match c with
        | 'R' -> TurnRight
        | 'L' -> TurnLeft
        | 'F' -> MoveForward
        | a -> failwithf "Unkown command %c" a

    let createRobotState (input:string) =
        printfn "||%s||" input
        Console.WriteLine(sprintf "||%s||" input)
        System.Diagnostics.Trace.WriteLine(sprintf "||%s||" input)

        let chars = input.Trim().Split ' ' |> Seq.map (char)
        let positions = chars |> Seq.take 2 |> Seq.map (int)
        let x = Seq.item 0 positions
        let y = Seq.item 1 positions
        let direction = chars |> Seq.last |> mapDirection
        ((x,y),direction)

    let createCommands (input:string) =
        input.ToCharArray() |> Seq.map mapCommand

    let initialRobotState = createRobotState (Seq.item 0 lines)
    let commands = createCommands(Seq.item 1 lines)
    let finalRobotState = createRobotState (Seq.item 2 lines)

    {InitialRobotState = initialRobotState; Commands = commands; FinalRobotState = finalRobotState}

let ProcessRobotCommands (initialRobotState, commands) =
    
    let rotateLeft((position,direction)) =
        match direction with
        | EAST -> (position, NORTH)
        | WEST -> (position, SOUTH)
        | NORTH -> (position, WEST)
        | SOUTH -> (position, EAST)

    let rotateRight((position,direction)) =
        match direction with
        | EAST -> (position, SOUTH)
        | WEST -> (position, NORTH)
        | NORTH -> (position, EAST)
        | SOUTH -> (position, WEST)

    let moveForward(((x,y),direction)) = 
        match direction with
        | EAST -> ((x+1,y), direction)
        | WEST -> ((x-1,y), direction)
        | NORTH -> ((x,y+1), direction)
        | SOUTH -> ((x,y-1), direction)

    let mapCommand(command) = 
        match command with
        | TurnLeft -> rotateLeft
        | TurnRight -> rotateRight
        | MoveForward -> moveForward

    let finalRobotState = Seq.fold (fun r c -> mapCommand c r) initialRobotState commands
    finalRobotState

[<Theory>]
[<InlineData(
    "1 1 E
    RFRFRFRF
    1 1 E")>]
[<InlineData(
    "3 2 N
    FRRFLLFFRRFLL
    3 3 N")>]
[<InlineData(
    "0 3 W
    LLFFFLFLFL
    2 4 S")>]
let Should_check_robot_journey (input) =
    let { InitialRobotState = initialState; Commands = commands; FinalRobotState = finalState } = CreateInput input
    let actual = ProcessRobotCommands (initialState, commands)
    Assert.Equal(finalState, actual)
