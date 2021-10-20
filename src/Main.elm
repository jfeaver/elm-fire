module Main exposing (..)

import Array exposing (Array)
import Browser exposing (Document)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Random
import Time


type Neighborhood
    = Mix (List Tree)
    | Only Int Tree (List Tree)


type NeighborSituation
    = NoNeighbors
    | Neighbors Neighborhood


type Tree
    = Living
    | Burning Int
    | Dead Int


type alias ForestRow =
    Array (Maybe Tree)


type alias Forest =
    Array ForestRow


type alias Model =
    { forest : Forest
    }


config =
    { burnTime = 1
    , deadTime = 3
    , neighborLocations =
        [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
    , pGrow = 0.05
    }


newModel : Model
newModel =
    { forest =
        [ [ Nothing, Just Living, Nothing, Nothing ]
        , [ Just (Burning 0), Nothing, Just Living, Nothing ]
        , [ Nothing, Just (Dead 0), Nothing, Just Living ]
        , [ Just Living, Nothing, Just (Burning 0), Nothing ]
        ]
            |> List.map Array.fromList
            |> Array.fromList
    }


main : Program () Model Msg
main =
    Browser.document
        { init = always ( newModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--- UPDATE ---


type Msg
    = Tick
    | Grow ( Int, Int ) Float -- Row, Column


rand =
    Random.float 0 1


probabilistically : (Float -> Msg) -> Cmd Msg
probabilistically toMsg =
    Random.generate toMsg rand


neighbors : Forest -> Int -> Int -> List Tree
neighbors forest row column =
    let
        getNeighbor : Forest -> ( Int, Int ) -> Maybe Tree
        getNeighbor forest_ ( dr, dc ) =
            forest_
                |> Array.get (row + dr)
                |> Maybe.andThen (Array.get (column + dc))
                |> Maybe.withDefault Nothing

        onlyTrees mTree trees =
            case mTree of
                Just tree ->
                    tree :: trees

                _ ->
                    trees
    in
    config.neighborLocations
        |> List.map (getNeighbor forest)
        |> List.foldl onlyTrees []


addNeighbor : Tree -> NeighborSituation -> NeighborSituation
addNeighbor tree situation =
    case situation of
        NoNeighbors ->
            Neighbors (Only 1 tree [ tree ])

        Neighbors neighborhood ->
            case neighborhood of
                Mix trees ->
                    Neighbors (Mix (tree :: trees))

                Only n onlyTree trees ->
                    let
                        oneMore =
                            Neighbors (Only (n + 1) onlyTree (tree :: trees))

                        newMix =
                            Neighbors (Mix (tree :: trees))
                    in
                    case tree of
                        Living ->
                            case onlyTree of
                                Living ->
                                    oneMore

                                _ ->
                                    newMix

                        Burning _ ->
                            case onlyTree of
                                Burning _ ->
                                    oneMore

                                _ ->
                                    newMix

                        Dead _ ->
                            case onlyTree of
                                Dead _ ->
                                    oneMore

                                _ ->
                                    newMix


neighborSituation : Forest -> Int -> Int -> NeighborSituation
neighborSituation forest row column =
    neighbors forest row column
        |> List.foldl addNeighbor NoNeighbors


cellTick : Forest -> Int -> Int -> Maybe Tree -> ( Maybe Tree, Cmd Msg )
cellTick forest row column mTree =
    case mTree of
        Just tree ->
            case tree of
                Living ->
                    case neighborSituation forest row column of
                        NoNeighbors ->
                            -- TODO: Randomly catch fire from new fire event
                            ( Just tree, Cmd.none )

                        Neighbors neighborhood ->
                            case neighborhood of
                                Mix neighborTrees ->
                                    -- TODO if any trees are burning then randomly catch fire from new event with compounding probability from multiple burning neighbors
                                    ( Just tree, Cmd.none )

                                Only n neighborTree neighborTrees ->
                                    case neighborTree of
                                        Living ->
                                            ( Just tree, Cmd.none )

                                        Burning _ ->
                                            -- TODO randomly catch fire from new event or with compounding probability from burning neighbors
                                            ( Just tree, Cmd.none )

                                        Dead _ ->
                                            ( Just tree, Cmd.none )

                Burning burnTime ->
                    if burnTime > config.burnTime then
                        ( Just (Dead 0), Cmd.none )

                    else
                        ( Just (Burning (burnTime + 1)), Cmd.none )

                Dead deadTime ->
                    if deadTime > config.deadTime then
                        -- STRETCH falling dead tree causes live tree to die
                        ( Nothing, Cmd.none )

                    else
                        ( Just (Dead (deadTime + 1)), Cmd.none )

        Nothing ->
            case neighborSituation forest row column of
                NoNeighbors ->
                    ( Nothing, probabilistically (Grow ( row, column )) )

                Neighbors neighborhood ->
                    case neighborhood of
                        Mix _ ->
                            ( Nothing, Cmd.none )

                        Only n tree neighborTrees ->
                            case tree of
                                Living ->
                                    -- TODO: Randomly grow a tree next to living trees
                                    ( Nothing, Cmd.none )

                                Dead _ ->
                                    -- TODO: Randomly grow a tree next to dead trees
                                    ( Nothing, Cmd.none )

                                _ ->
                                    ( Nothing, Cmd.none )


forestRowTick : Forest -> Int -> ForestRow -> ( ForestRow, Cmd Msg )
forestRowTick forest row forestRow =
    let
        mergeTree : Maybe Tree -> ( Int, ForestRow, Cmd Msg ) -> ( Int, ForestRow, Cmd Msg )
        mergeTree mTree ( column, tickForestRow, cmd ) =
            let
                ( tickTree, tickCmd ) =
                    cellTick forest row column mTree
            in
            ( column + 1, Array.push tickTree tickForestRow, Cmd.batch [ cmd, tickCmd ] )

        ( _, nextRow, nextCmd ) =
            Array.foldl mergeTree ( 0, Array.empty, Cmd.none ) forestRow
    in
    ( nextRow, nextCmd )


forestTick : Forest -> ( Forest, Cmd Msg )
forestTick forest =
    let
        mergeRow : ForestRow -> ( Int, Forest, Cmd Msg ) -> ( Int, Forest, Cmd Msg )
        mergeRow forestRow ( row, newForest, cmd ) =
            let
                ( tickForestRow, tickCmd ) =
                    forestRowTick forest row forestRow
            in
            ( row + 1, Array.push tickForestRow newForest, Cmd.batch [ cmd, tickCmd ] )

        ( _, nextForest, nextCmd ) =
            Array.foldl mergeRow ( 0, Array.empty, Cmd.none ) forest
    in
    ( nextForest, nextCmd )


forestSet : ( Int, Int ) -> Maybe Tree -> Forest -> Forest
forestSet ( row, column ) mTree forest =
    forest
        |> Array.get row
        |> Maybe.map (Array.set column mTree)
        |> Maybe.map (\forestRow -> Array.set row forestRow forest)
        |> Maybe.withDefault forest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            let
                ( forest, cmd ) =
                    forestTick model.forest
            in
            ( { model | forest = forest }, cmd )

        Grow position rng ->
            if rng < config.pGrow then
                ( { model | forest = forestSet position (Just Living) model.forest }, Cmd.none )

            else
                ( model, Cmd.none )



--- VIEW ---


cellWidth =
    20


color =
    { living = rgb 0 256 0
    , fire = rgb 256 0 0
    , ash = rgb 155 145 125
    , white = rgb 256 256 256
    , black = rgb 0 0 0
    }


forestCellView : Maybe Tree -> Html Msg
forestCellView maybeTree =
    let
        cellCss bc =
            [ width (px cellWidth), height (px cellWidth), backgroundColor bc ]
    in
    case maybeTree of
        Just tree ->
            case tree of
                Living ->
                    span [ css (cellCss <| color.living) ] []

                Burning _ ->
                    span [ css (cellCss <| color.fire) ] []

                Dead _ ->
                    span [ css (cellCss <| color.ash) ] []

        Nothing ->
            span [ css (cellCss <| color.white) ] []


forestRowView : ForestRow -> Html Msg
forestRowView forestRow =
    forestRow
        |> Array.toList
        |> List.map forestCellView
        |> div [ css [ displayFlex ] ]


forestView : Forest -> Html Msg
forestView forest =
    forest
        |> Array.toList
        |> List.map forestRowView
        |> div [ css [] ]


documentHtml : Model -> List (Html Msg)
documentHtml model =
    [ div
        [ css
            [ display inlineBlock
            , margin (px 20)
            , border3 (px 2) solid color.black
            ]
        ]
        [ forestView model.forest ]
    ]


view : Model -> Document Msg
view model =
    Document "Elm Forest Fire Simulator"
        (model
            |> documentHtml
            |> List.map toUnstyled
        )



--- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions =
    always <| Time.every 250 (always Tick)
