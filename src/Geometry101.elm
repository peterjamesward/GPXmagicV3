module Geometry101 exposing (Circle, Column, LineEquation, Matrix, Point, Road, distance, findIntercept, interpolateLine, isAfter, isBefore, lineCircleIntersections, lineEquationFromTwoPoints, lineIntersection, linePerpendicularTo, pointAlongRoad, pointsToGeometry)

import List


type alias Point =
    { x : Float
    , y : Float
    }


type alias Road =
    { startAt : Point
    , endsAt : Point
    }


type alias Circle =
    { centre : Point
    , radius : Float
    }


type alias LineEquation =
    { a : Float
    , b : Float
    , c : Float
    }


type alias Matrix =
    { tl : Float
    , tr : Float
    , bl : Float
    , br : Float
    }


type alias Column =
    { t : Float
    , b : Float
    }


distance p1 p2 =
    sqrt <| (p1.x - p2.x) ^ 2.0 + (p1.y - p2.y) ^ 2.0


pointsToGeometry : Point -> Point -> Road
pointsToGeometry p1 p2 =
    { startAt = p1, endsAt = p2 }


isBefore : Road -> Point -> Bool
isBefore r p =
    antiInterpolate p r.startAt r.endsAt < 0.0


isAfter : Road -> Point -> Bool
isAfter r p =
    antiInterpolate p r.startAt r.endsAt > 1.0


pointAlongRoad : Road -> Float -> Point
pointAlongRoad road distanceFromStart =
    let
        roadLength =
            distance road.startAt road.endsAt
    in
    interpolateLine
        (distanceFromStart / roadLength)
        road.startAt
        road.endsAt


interpolateScalar fraction a b =
    b * fraction + a * (1.0 - fraction)


interpolateLine fraction p1 p2 =
    -- Find p3 on p1-p2, such that |p1p3|/|p1p2| = fraction
    -- Expecting fraction to be in [0,1] but doesn't have to be.
    { x = interpolateScalar fraction p1.x p2.x
    , y = interpolateScalar fraction p1.y p2.y
    }


antiInterpolate : Point -> Point -> Point -> Float
antiInterpolate p pa pb =
    -- Supports testing for converging roads.
    -- Express p as fraction along AB so at A is 0, at B is 1.
    -- Assumes points are co-linear, meaningless otherwise.
    let
        aDist =
            distance p pa

        bDist =
            distance p pb

        ab =
            distance pa pb
    in
    if aDist + bDist <= ab then
        -- Interior point
        aDist / ab

    else if aDist > bDist then
        -- It's outboard on the B side
        aDist / ab

    else if bDist > aDist then
        -1.0 * (aDist / ab)

    else
        -- No idea where it is
        0.0



{-
   This is about helping to smooth a bend.
   In one case, maybe are looking at only one vertex.
   That is to say, we have one road segment B-P and the next P-C.
   Then we have the three points we need to find the incenter and incircle.
   Note that we are implicitly.

   More generally, suppose we have segments
   AB, BC, CD, DE, EF, FG
   We select at least two contiguous segments.
   Suppose we select (BC, CD).
   This means that points B and D remain, point C will be replaced with
   new segments derived from the incircle of BCD.
   We don't need to find the intersection of BC and CD, we know it.

   Suppose we select (BC, CD, DE).
   This means that B and E remain, C and D will be replaced with
   new segments derived from the incircle of BCP,
   where P is the intersect of (the extensions of) BC and DE.
-}


findIntercept : Road -> Road -> Maybe Point
findIntercept r1 r2 =
    {-
       The intercept P of AB and CD, if it exists, satisfies both equations.

           0 x + 2 y -10 == 0
       &&  2 x - 2 y -2  == 0

       In matrix form  | 0 2  | | x |    | -10 |
                       | 2 -2 | | y | == |  +2 |

       By inverting and multiplying through, the intersect P is
       | x | = | 4 |
       | y |   | 5 |

       We now have three points:
       B = (3,5)    C = (4,3)   P = (4,5)


       Now let us try to draw this circle on the third person view!
    -}
    let
        r1Line =
            lineEquationFromTwoPoints r1.startAt r1.endsAt

        r2Line =
            lineEquationFromTwoPoints r2.startAt r2.endsAt
    in
    lineIntersection r1Line r2Line


lineIntersection : LineEquation -> LineEquation -> Maybe Point
lineIntersection l1 l2 =
    let
        matrix =
            { tl = l1.a
            , tr = l1.b
            , bl = l2.a
            , br = l2.b
            }

        inv =
            matrixInverse matrix
    in
    case inv of
        Just inverse ->
            let
                column =
                    { t = -1.0 * l1.c, b = -1.0 * l2.c }

                col =
                    matrixMultiplyColumn inverse column
            in
            Just { x = col.t, y = col.b }

        Nothing ->
            Nothing


solveQuadratic : Float -> Float -> Float -> List Float
solveQuadratic a b c =
    let
        disc =
            b * b - 4 * a * c
    in
    if disc == 0 then
        [ -(b / (a + a)) ]

    else if disc > 0 then
        [ (-b - sqrt disc) / (a + a)
        , (-b + sqrt disc) / (a + a)
        ]

    else
        []


lineCircleIntersections : LineEquation -> Circle -> List Point
lineCircleIntersections { a, b, c } { centre, radius } =
    -- Line equation is Ax + By + C = 0.
    -- Circle is in { centre, radius } form
    let
        shiftedLine =
            -- Shift so that we can use centre of circle as origin pro tem.
            { a = a, b = b, c = c + (a * centre.x + b * centre.y) }

        xSolutionsShifted =
            -- We can solve a quadratic, to yield 0, 1, or 2 x values.
            solveQuadratic
                (shiftedLine.a * shiftedLine.a + shiftedLine.b * shiftedLine.b)
                (2.0 * shiftedLine.a * shiftedLine.c)
                (shiftedLine.c * shiftedLine.c - shiftedLine.b * shiftedLine.b * radius * radius)

        xSolutions =
            List.map ((+) centre.x) xSolutionsShifted

        ySolutions =
            List.map (\x -> -((a * x + c) / b)) xSolutions
    in
    List.map2 Point xSolutions ySolutions


matrixInverse : Matrix -> Maybe Matrix
matrixInverse m =
    let
        determinant =
            m.tl * m.br - m.tr * m.bl
    in
    if abs determinant < 10 ^ -20 then
        Nothing

    else
        Just
            { tl = m.br / determinant
            , tr = -1.0 * m.tr / determinant
            , bl = -1.0 * m.bl / determinant
            , br = m.tl / determinant
            }


matrixMultiplyColumn : Matrix -> Column -> Column
matrixMultiplyColumn m c =
    { t = m.tl * c.t + m.tr * c.b
    , b = m.bl * c.t + m.br * c.b
    }


lineEquationFromTwoPoints : Point -> Point -> LineEquation
lineEquationFromTwoPoints p1 p2 =
    {-
       An arrangement of the two point line equation is:
       (y1 - y2) X + (x2 - x1) Y + (x1.y2 - x2.y1) = 0

       For AB this is
       (5.0 - 5.0) X + (3.0 - 1.0) Y + (5.0 - 15.0) = 0
       Thus A = 0, B = 2, C = -10

       To check, for (1,5) : 0 x 1 + 2 x 5 + (-10) == 0
                 for (3,5) : 0 x 3 + 2 x 5 + (-10) == 0

       For CD:
       (3.0 - 1.0) X + (2.0 - 4.0) Y + (4.0 - 6.0) = 0
       Thus A = 2, B = -2, C = -2

       To check, for (4,3) : 2 x 4 + (-2) x 3 + (-2) == 0
                 for (2,1) : 2 x 2 + (-2) x 1 + (-2) == 0
    -}
    let
        a =
            p1.y - p2.y

        b =
            p2.x - p1.x

        c =
            p1.x * p2.y - p2.x * p1.y
    in
    { a = a, b = b, c = c }


linePerpendicularTo : LineEquation -> Point -> LineEquation
linePerpendicularTo line p =
    -- Perpendicular passing through point.
    let
        aybx =
            line.a * p.y - line.b * p.x
    in
    { a = line.b, b = -1.0 * line.a, c = aybx }
