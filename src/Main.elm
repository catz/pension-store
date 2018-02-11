module Main exposing (..)

import Html exposing (..)
import ToFixed exposing (..)
import Html exposing (i, span, div, button)
import Html.Attributes exposing (class, classList, colspan)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Button as Button
import List.Extra exposing (updateIf)
import Navigation


---- MODEL ----


type alias Product =
    { id : Int
    , description : String
    , quantityInStock : Int
    , quantityInCart : Int
    , price : Int
    }


type alias Model =
    { availableProducts : List Product, page : Page }


type Page
    = Home
    | Checkout


cartTotal : Model -> Int
cartTotal model =
    model.availableProducts
        |> List.map (\item -> item.price * item.quantityInCart)
        |> List.sum


cartList : Model -> List Product
cartList model =
    List.filter (\product -> product.quantityInCart > 0) model.availableProducts


isCartEmpty : Model -> Bool
isCartEmpty model =
    cartTotal model == 0


initialModel : Model
initialModel =
    { availableProducts =
        [ { id = 1, description = "Casio Men's W800H-1AV Classic Sport Watch with Black Band", quantityInStock = 10, price = 1149, quantityInCart = 0 }
        , { id = 2, description = "Halo II Headband Sweatband Pullover", quantityInStock = 8, price = 1495, quantityInCart = 0 }
        , { id = 3, description = "Nike Men's Tech Essential Web Belt", quantityInStock = 5, price = 720, quantityInCart = 0 }
        , { id = 4, description = "Under Armour 6\" Performance Wristband", quantityInStock = 3, price = 899, quantityInCart = 0 }
        ]
    , page = Home
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( initialModel
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = AddToCart Product
    | RemoveFromCart Product
    | ProceedCheckout
    | ContinueShopping
    | UrlChange Navigation.Location


addToCart : Product -> Product
addToCart product =
    { product | quantityInStock = product.quantityInStock - 1, quantityInCart = product.quantityInCart + 1 }


removeFromCart : Product -> Product
removeFromCart product =
    { product | quantityInStock = product.quantityInStock + product.quantityInCart, quantityInCart = 0 }


getPage : String -> Page
getPage hash =
    case hash of
        "#home" ->
            Home

        "#checkout" ->
            Checkout

        _ ->
            Home


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            ( { model | page = (getPage location.hash) }, Cmd.none )

        ProceedCheckout ->
            ( model, Navigation.newUrl "#checkout" )

        ContinueShopping ->
            ( model, Navigation.newUrl "#home" )

        RemoveFromCart product ->
            let
                newAvailableProducts =
                    updateIf ((==) product) removeFromCart model.availableProducts
            in
                ( { model | availableProducts = newAvailableProducts }, Cmd.none )

        AddToCart ({ quantityInStock } as product) ->
            case quantityInStock of
                0 ->
                    ( model, Cmd.none )

                _ ->
                    let
                        newAvailableProducts =
                            updateIf ((==) product) addToCart model.availableProducts
                    in
                        ( { model | availableProducts = newAvailableProducts }, Cmd.none )



---- VIEW ----


formatPrice : Int -> String
formatPrice price =
    toFixed 2 <| toFloat price / 100


viewProduct : Product -> Table.Row Msg
viewProduct product =
    Table.tr []
        [ Table.td [] [ text <| toString product.id ]
        , Table.td [] [ text product.description ]
        , Table.td []
            [ sup [] [ text "$" ]
            , span [] [ text <| formatPrice product.price ]
            ]
        , Table.td [] [ text <| toString product.quantityInStock ]
        , Table.td []
            [ Button.button [ Button.small, Button.info, Button.onClick <| AddToCart product ]
                [ text "Add to cart"
                , i [ class "fa fa-shopping-cart" ] []
                ]
            ]
        ]


viewAvailableProducts : Model -> Grid.Column Msg
viewAvailableProducts model =
    Grid.col []
        [ h2 [] [ text "Products" ]
        , Table.table
            { options = [ Table.hover ]
            , thead =
                Table.thead []
                    [ Table.tr []
                        [ Table.th [] [ text "ID" ]
                        , Table.th [] [ text "Product Name" ]
                        , Table.th [] [ text "Price" ]
                        , Table.th [] [ text "In Stock" ]
                        , Table.th [] [ text "Transactions" ]
                        ]
                    ]
            , tbody = Table.tbody [] (List.map viewProduct model.availableProducts)
            }
        ]


viewCart : Model -> Grid.Column Msg
viewCart model =
    let
        tbody =
            if isCartEmpty model then
                [ Table.tr []
                    [ Table.td [ Table.cellAttr (colspan 5), Table.cellAttr (class "text-center") ] [ text "Your shopping cart is empty." ]
                    ]
                ]
            else
                cartList model
                    |> List.map viewCartProduct

        checkout =
            if isCartEmpty model then
                div [] []
            else
                Button.button [ Button.primary, Button.block, Button.large, Button.onClick <| ProceedCheckout ]
                    [ text "Checkout"
                    , i [ class "fa fa-angle-right" ] []
                    ]
    in
        Grid.col []
            [ h2 [] [ text "My Shopping Cart" ]
            , Table.table
                { options = [ Table.hover ]
                , thead =
                    Table.thead []
                        [ Table.tr []
                            [ Table.th [] [ text "ID" ]
                            , Table.th [] [ text "Product Name" ]
                            , Table.th [] [ text "Price" ]
                            , Table.th [] [ text "Quantity" ]
                            ]
                        ]
                , tbody = Table.tbody [] tbody
                }
            , div [ class "clearfix" ] []
            , span [ class "pull-right alert alert-success" ]
                [ span [] [ text "Total Price: " ]
                , sup [] [ text "$" ]
                , span [] [ text <| formatPrice <| cartTotal model ]
                ]
            , checkout
            ]


viewCartProduct : Product -> Table.Row Msg
viewCartProduct product =
    Table.tr []
        [ Table.td [] [ text <| toString product.id ]
        , Table.td [] [ text product.description ]
        , Table.td []
            [ sup [] [ text "$" ]
            , span [] [ text <| formatPrice product.price ]
            ]
        , Table.td []
            [ text <| toString product.quantityInCart ]
        , Table.td []
            [ Button.button [ Button.small, Button.danger, Button.onClick <| RemoveFromCart product ]
                [ text ""
                , i [ class "fa fa-remove" ] []
                ]
            ]
        ]


viewCheckout : Model -> Grid.Column Msg
viewCheckout model =
    Grid.col []
        [ h2 [] [ text "Checkout" ]
        , span [ class "pull-right alert alert-success" ]
            [ span [] [ text "Total Price: " ]
            , sup [] [ text "$" ]
            , span [] [ text <| formatPrice <| cartTotal model ]
            ]
        , Button.button [ Button.small, Button.info, Button.large, Button.onClick <| ContinueShopping ]
            [ text "Continue Shopping"
            , i [ class "fa fa-shopping-cart" ] []
            ]
        , Button.button [ Button.small, Button.primary, Button.large ]
            [ text "Done"
            , i [ class "fa fa-angle-right" ] []
            ]
        ]


view : Model -> Html Msg
view model =
    let
        body =
            case model.page of
                Home ->
                    Grid.row []
                        [ viewAvailableProducts model
                        , viewCart model
                        ]

                Checkout ->
                    Grid.row []
                        [ viewCheckout model ]
    in
        Grid.container []
            [ CDN.stylesheet
            , body
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
