module Main exposing (..)

import Browser exposing (Document)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
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
    List (Maybe Tree)


type alias Forest =
    List ForestRow


type alias Model =
    { forest : Forest
    }


config =
    { burnTime = 1
    , deadTime = 3
    }


newModel : Model
newModel =
    { forest =
        [ [ Nothing, Just Living, Nothing, Nothing ]
        , [ Just (Burning 0), Nothing, Just Living, Nothing ]
        , [ Nothing, Just (Dead 0), Nothing, Just Living ]
        , [ Just Living, Nothing, Just (Burning 0), Nothing ]
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = always ( newModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Tick


neighbors : Forest -> Int -> Int -> List Tree
neighbors forest row column =
    -- TODO
    []


neighborSituation : Forest -> Int -> Int -> NeighborSituation
neighborSituation forest row column =
    -- TODO
    neighbors forest row column
        |> always NoNeighbors


cellTick : Forest -> Int -> Int -> Maybe Tree -> Maybe Tree
cellTick forest row column mTree =
    let
        situation =
            neighborSituation forest row column
    in
    case mTree of
        Just tree ->
            case tree of
                Living ->
                    case situation of
                        NoNeighbors ->
                            -- TODO: Randomly catch fire from new fire event
                            Just tree

                        Neighbors neighborhood ->
                            case neighborhood of
                                Mix neighborTrees ->
                                    -- TODO if any trees are burning then randomly catch fire from new event with compounding probability from multiple burning neighbors
                                    -- STRETCH falling dead tree causes live tree to die
                                    Just tree

                                Only n neighborTree neighborTrees ->
                                    case neighborTree of
                                        Living ->
                                            Just tree

                                        Burning _ ->
                                            -- TODO randomly catch fire from new event or with compounding probability from burning neighbors
                                            Just tree

                                        Dead _ ->
                                            -- STRETCH falling dead tree causes live tree to die
                                            Just tree

                Burning burnTime ->
                    if burnTime > config.burnTime then
                        Just (Dead 0)

                    else
                        Just (Burning (burnTime + 1))

                Dead deadTime ->
                    if deadTime > config.deadTime then
                        Nothing

                    else
                        Just (Dead (deadTime + 1))

        Nothing ->
            case situation of
                NoNeighbors ->
                    -- TODO: Randomly grow a tree that is in the open
                    Nothing

                Neighbors neighborhood ->
                    case neighborhood of
                        Mix _ ->
                            Nothing

                        Only n tree neighborTrees ->
                            case tree of
                                Living ->
                                    -- TODO: Randomly grow a tree next to living trees
                                    Nothing

                                _ ->
                                    Nothing


forestTick : Forest -> Forest
forestTick forest =
    let
        forestRowTick row =
            List.indexedMap (cellTick forest row)
    in
    List.indexedMap forestRowTick forest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | forest = forestTick model.forest }, Cmd.none )



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
        |> List.map forestCellView
        |> div [ css [ displayFlex ] ]


forestView : Forest -> Html Msg
forestView forest =
    forest
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
