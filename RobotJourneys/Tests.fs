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
    let lines = input.Split( [|"\r\n"|], StringSplitOptions.RemoveEmptyEntries) |> Seq.map (fun s-> s.Trim())

    let MapDirection (c) =
        match c with
        | 'E' -> EAST
        | 'W' -> WEST
        | 'N' -> NORTH
        | 'S' -> SOUTH

    let MapCommand (c) =
        match c with
        | 'R' -> TurnRight
        | 'L' -> TurnLeft
        | 'F' -> MoveForward

    let CreateRobotState (input:string) =
        let chars = input.Split ' ' |> Seq.map (char)
        let positions = chars |> Seq.take 2 |> Seq.map (int)
        let x = Seq.item 0 positions
        let y = Seq.item 1 positions
        let direction = chars |> Seq.last |> MapDirection
        ((x,y),direction)

    let CreateCommands (input:string) =
        input.ToCharArray() |> Seq.map MapCommand

    let initialRobotState = CreateRobotState (Seq.item 0 lines)
    let commands = CreateCommands(Seq.item 1 lines)
    let finalRobotState = CreateRobotState (Seq.item 2 lines)

    {InitialRobotState = initialRobotState; Commands = commands; FinalRobotState = finalRobotState}

let ProcessRobotCommands (initialRobotState, commands) =
    
    let RotateLeft((position,direction)) =
        match direction with
        | EAST -> (position, NORTH)
        | WEST -> (position, SOUTH)
        | NORTH -> (position, WEST)
        | SOUTH -> (position, EAST)

    let RotateRight((position,direction)) =
        match direction with
        | EAST -> (position, SOUTH)
        | WEST -> (position, NORTH)
        | NORTH -> (position, EAST)
        | SOUTH -> (position, WEST)

    let MoveForward(((x,y),direction)) = 
        match direction with
        | EAST -> ((x+1,y), direction)
        | WEST -> ((x-1,y), direction)
        | NORTH -> ((x,y+1), direction)
        | SOUTH -> ((x,y-1), direction)

    let MapCommand(command) = 
        match command with
        | TurnLeft -> RotateLeft
        | TurnRight -> RotateRight
        | MoveForward -> MoveForward

    let finalRobotState = Seq.fold (fun r c -> MapCommand c r) initialRobotState commands
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
