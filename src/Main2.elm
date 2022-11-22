module Main2 exposing (..)
import Browser
import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Svg as S
import Svg.Attributes as SA
import Maybe as M
import List as L

g = -9.81

getRange : Float -- hoop height
        -> Float -- starting height
        -> Float -- veloctiy x at 0
        -> Float -- velocity y at 0
        -> Float -- distance hoop should be placed to go in
getRange hh y0 vx vy = let time = (vy + sqrt (vy^2 - 4 * (1/2) * g * (y0-hh)))/(-g)
                       in vx * time

initialVelocity : Float -- hoop height
               -> Float -- starting height
               -> Float -- hoop distance
               -> Float -- theta
               -> Float -- initial velocity
initialVelocity hh y0 d theta = sqrt ((1/2 * g * d^2) / ( cos theta ^ 2 * (hh - y0 - (tan theta * d ))))

initialAngle : Float -- hoop height
            -> Float -- starting height
            -> Float -- velocity initial maginitude
            -> Float -- distance
            -> Float -- angle
initialAngle hh y0 v0 d =
        let a = (1/2 * g * d^2)/(v0^2)
            dh = hh-y0
        in atan ( (d + sqrt (d^2 - 4 * a * (a - dh)))/(-2 * a))

-- ui

type SubModel = GetDistance {vm : String, theta : String} | GetVelocityMagnitude { distance : String, theta : String} | GetAngle {distance : String, vm : String}

type alias Model = {hoopHeight : String, startingHeight : String, problem : SubModel}

type Msg = UpdateHoopHeight String | UpdateStartHeight String | UpdateVel String | UpdateAngle String | UpdateDistance String | UpdateProblem String

setVelocity : String -> Model -> Model
setVelocity v m@{problem} = case problem of
    GetDistance a -> {m | problem = GetDistance {a | vm = v}}
    GetAngle a -> {m | problem = GetAngle {a | vm = v}}
    GetVelocityMagnitude v -> m


setVelocity : String -> Model -> Model
setVelocity v m@{problem} = case problem of
    GetDistance a -> {m | problem = GetDistance {a | vm = v}}
    GetAngle a -> {m | problem = GetAngle {a | vm = v}}
    GetVelocityMagnitude v -> m

setVelocity : String -> Model -> Model
setVelocity v m@{problem} = case problem of
    GetDistance a -> {m | problem = GetDistance {a | vm = v}}
    GetAngle a -> {m | problem = GetAngle {a | vm = v}}
    GetVelocityMagnitude v -> m

update : Msg -> Model -> Model
update msg mod = case msg of
    UpdateHoopHeight h -> {mod | hoopHeight h}
    UpdateStartHeight h -> {mod | startingHeight h}
    UpdateVel v -> setVelocity mod v
    UpdateAngle a -> setAngle mod a
    UpdateDistance d -> setDistance d
    UpdateProblem p -> changeProblem mod p


mbind : Maybe a -> (a -> Maybe b) -> Maybe b
mbind ma f = case ma of
    Nothing -> Nothing
    Just a -> f a


type alias SvgParams = {
        hoopHeight : Float,
        startingHeight : Float,
        velMagnitude : Float,
        launchAngle : Float,
        distance : Float
    }

drawSvgModel : Maybe SvgParams -> S.Svg Msg
drawSvgModel msvgp = 
    S.svg [SA.width "800",
           SA.height "600",
           SA.viewBox "0 0 800 600"]
          (case msvgp of
            Nothing -> [S.text_ [SA.textAnchor "center", SA.x "400", SA.y "300"] [S.text "invalid input"]]
            Just sp -> let subAmount = Basics.min (sp.startingHeight - 8/100) (sp.hoopHeight - 4/100)
                           scale = 800/(sp.distance + 10/100 + 4/100)
                           hys = String.fromFloat hy
                           hy = 600 - (sp.hoopHeight - subAmount) * scale
                           hx = 20/100 * scale
                           hxs = String.fromFloat hx
                           by = 600 - (sp.startingHeight - subAmount) * scale
                           bx = 800 - (4/100 * scale)
                           angle = sp.launchAngle * pi/180
                           vy = sin angle * sp.velMagnitude
                           vx = cos angle * sp.velMagnitude
                       in [ S.line [ SA.x1 "0", SA.y1 "600", SA.x2 "0", SA.y2 hys, SA.stroke "black"] [],
                            S.line [ SA.x1 "0", SA.y1 hys, SA.x2 hxs, SA.y2 hys, SA.stroke "black"] [],
                            S.circle [SA.cx (String.fromFloat bx), SA.cy (String.fromFloat by), SA.r (String.fromFloat (scale * 4/100))] [],
                            S.polyline [SA.points (String.concat (L.map (\xlocPix ->
                                                let currentXpos = (bx-xlocPix)/scale
                                                    time = currentXpos/vx
                                                    cy = sp.startingHeight + vy * time + 0.5 * g * time^2 - subAmount
                                                    cyp = 600 - (cy * scale)
                                                in
                                                    String.fromFloat xlocPix ++ "," ++ String.fromFloat cyp ++ " "
                                                    ) (L.map (\a -> toFloat a * 6.0) (L.range 0 (floor (bx/6)))))), SA.stroke "black", SA.fill "none"] []])


-- x = vx * t

problemOpt : String -> Html.Html Msg
problemOpt s = option [ value s ] [ text s ]

displayResult : Maybe Float -> Html.Html Msg
displayResult r = text (case r of
    Nothing -> "NaN"
    Just rn -> String.fromFloat rn)

view : Model -> Html.Html Msg
view  {hoopHeight, startingHeight, problem}=
    let hoopInput = input [ placeholder "hoop height",  onInput UpdateHoopHeight] []

        startHeightInput = input [ placeholder "start height",  onInput UpdateStartHeight] []
        initvelInput = input [ placeholder "initial velocity",  onInput UpdateVel] []
        angInput = input [ placeholder "angle",  onInput UpdateAngle] []
        distanceInput = input [ placeholder "distance", onInput UpdateDistance] []
        problemInput = select [ onInput UpdateProblem] [problemOpt "angle", problemOpt "distance", problemOpt "speed"]
    in
    div [] ([problemInput] ++ (case problem of
        GetDistance {vm,theta} -> let r = M.map getRange hh sh v a
                                      hh = String.toFloat hoopHeight
                                      sh = String.toFloat startingHeight
                                      v = String.toFloat vm
                                      a = String.toFloat theta
                                  in [displayResult r, hr [] [], hoopInput, startHeightInput, initvelInput, angInput,
                                      drawSvgModel (M.map5 SvgParams hh sh v a r)]
        GetAngle {distance,vm} -> let a = M.map initialAngle hh sh r v
                                      hh = String.toFloat hoopHeight
                                      sh = String.toFloat startingHeight
                                      v = String.toFloat vm
                                      r = String.toFloat distance
                                  in [displayResult a, hr [] [], hoopInput, startHeightInput, initvelInput, distanceInput,
                                      drawSvgModel (M.map5 SvgParams hh sh v a r)]
        GetVelocityMagnitude {distance,theta} ->
                let v = M.map initialVelocity hh sh r a
                    hh = String.toFloat hoopHeight
                    sh = String.toFloat startingHeight
                    a = String.toFloat theta
                    r = String.toFloat distance
                in [displayResult v, hr [] [], hoopInput, startHeightInput, angInput, distanceInput,
                    drawSvgModel (M.map5 SvgParams hh sh v a r)]
       ))

    -- div []
    -- [ text (case r of
    --     Nothing -> "NaN"
    --     Just a -> String.fromFloat a)
    -- , hr [] []
    -- , hoopInput
    -- , startHeightInput
    -- , initvelInput
    -- , angInput
    -- , drawSvgModel (M.map5 SvgParams (String.toFloat hoopHeight) (String.toFloat startingHeight) (String.toFloat vm) (String.toFloat theta) r)
    -- ]

main = Browser.sandbox {
        init = {hoopHeight = "1.915", startingHeight = "1.61", vm = "3.3931", theta = "30"},
        view = view,
        update = update
    }
