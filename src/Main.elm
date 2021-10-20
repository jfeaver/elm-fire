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
    let
        deltaT =
            50

        multiplier =
            250.0 / deltaT
    in
    { deltaT = deltaT
    , burnTime = Basics.round (multiplier * 1)
    , deadTime = Basics.round (multiplier * 3)
    , neighborLocations =
        [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
    , pGrow = 0.05
    , growDropOff = 1.3 -- 1.3 is good for 4 neighbor locations, 1 is good for 8
    , pCatchFire = 0.005
    , pSpreadFire = 0.15
    , spreadFireRampUp = 1
    }


dropOff dropOff_ n =
    1.0 / (1.0 + e ^ (dropOff_ * n - 4))


{-| complementary events
-}
rampUp pOrig n =
    1.0 - ((1.0 - pOrig) ^ n)


newModel : Model
newModel =
    { forest =
        [ [ Nothing, Just Living, Just (Burning 0), Nothing ]
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
    | Grow ( Int, Int ) Float Float -- Row, Column
    | CatchFire ( Int, Int ) Float Float


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


countBurningTrees : List Tree -> Int
countBurningTrees trees =
    let
        counter tree count =
            case tree of
                Burning _ ->
                    count + 1

                _ ->
                    count
    in
    List.foldl counter 0 trees


cellTick : Forest -> Int -> Int -> Maybe Tree -> ( Maybe Tree, Cmd Msg )
cellTick forest row column mTree =
    case mTree of
        Just tree ->
            case tree of
                -- TODO: Randomly die of natural causes in all of these cases
                Living ->
                    case neighborSituation forest row column of
                        NoNeighbors ->
                            ( Just tree, probabilistically (CatchFire ( row, column ) config.pCatchFire) )

                        Neighbors neighborhood ->
                            case neighborhood of
                                Mix neighborTrees ->
                                    -- TODO if any trees are burning then randomly catch fire from new event with compounding probability from multiple burning neighbors
                                    let
                                        burnCount =
                                            countBurningTrees neighborTrees

                                        pSpreadFire =
                                            rampUp config.pSpreadFire (toFloat burnCount)
                                    in
                                    if burnCount > 0 then
                                        ( Just tree, probabilistically (CatchFire ( row, column ) pSpreadFire) )

                                    else
                                        ( Just tree, Cmd.none )

                                Only n neighborTree neighborTrees ->
                                    case neighborTree of
                                        Living ->
                                            -- TODO: Perhaps a dense forest decreases the chances of catching fire slightly
                                            ( Just tree, probabilistically (CatchFire ( row, column ) config.pCatchFire) )

                                        Burning _ ->
                                            let
                                                pSpreadFire =
                                                    rampUp config.pSpreadFire (toFloat n)
                                            in
                                            ( Just tree, probabilistically (CatchFire ( row, column ) pSpreadFire) )

                                        Dead _ ->
                                            ( Just tree, Cmd.none )

                Burning burnTime ->
                    if burnTime > config.burnTime then
                        ( Just (Dead 0), Cmd.none )

                    else
                        ( Just (Burning (burnTime + 1)), Cmd.none )

                Dead deadTime ->
                    -- TODO: Randomly catch fire with higher probability in each of these cases
                    -- TODO: Randomly catch fire from burning neighbor with very high probability
                    if deadTime > config.deadTime then
                        -- STRETCH falling dead tree causes live tree to die
                        ( Nothing, Cmd.none )

                    else
                        ( Just (Dead (deadTime + 1)), Cmd.none )

        Nothing ->
            case neighborSituation forest row column of
                NoNeighbors ->
                    ( Nothing, probabilistically (Grow ( row, column ) config.pGrow) )

                Neighbors neighborhood ->
                    case neighborhood of
                        Mix _ ->
                            ( Nothing, Cmd.none )

                        Only n tree neighborTrees ->
                            case tree of
                                Living ->
                                    let
                                        pGrow =
                                            config.pGrow * dropOff config.growDropOff (toFloat n)
                                    in
                                    ( Nothing, probabilistically (Grow ( row, column ) pGrow) )

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

        Grow position pGrow rng ->
            if rng < pGrow then
                ( { model | forest = forestSet position (Just Living) model.forest }, Cmd.none )

            else
                ( model, Cmd.none )

        CatchFire position pCatchFire rng ->
            if rng < pCatchFire then
                ( { model | forest = forestSet position (Just (Burning 0)) model.forest }, Cmd.none )

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
    always <| Time.every config.deltaT (always Tick)
