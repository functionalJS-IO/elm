module Tests exposing (tests)

import Expect
import Test exposing (..)
import Zipper
    exposing
        ( BinTree(..)
        , fromTree
        , left
        , right
        , setLeft
        , setRight
        , setValue
        , toTree
        , up
        , value
        )


t1 =
    Node (Node Leaf 2 (Node Leaf 3 Leaf)) 1 (Node Leaf 4 Leaf)



-- t2 =
--     Node 1 (node 5 Nothing $ leaf 3) $ leaf 4
--
--
-- t3 =
--     Node 1 (node 2 (leaf 5) $ leaf 3) $ leaf 4
--
--
-- t4 =
--     Node 1 (leaf 2) $ leaf 4
--
--
-- t5 =
--     Node 6 (leaf 7) $ leaf 8
--
--
-- t6 =
--     Node 1 (node 2 Nothing $ leaf 3) $ node 6 (leaf 7) (leaf 8)
--
--
-- t7 =
--     Node 1 (node 2 Nothing $ leaf 5) $ leaf 4


tests : Test
tests =
    describe "Zipper"
        [ test "data is retainer" <|
            \_ ->
                toTree (fromTree t1)
                    |> Expect.equal t1
        , test "left, right and value" <|
            \_ ->
                t1
                    |> fromTree
                    |> left
                    |> Maybe.andThen right
                    |> Maybe.andThen value
                    |> Expect.equal (Just 3)
        , test "dead end" <|
            \_ ->
                t1
                    |> fromTree
                    |> left
                    |> Maybe.andThen left
                    |> Expect.equal Nothing
        ]



--     test "traversing up from top" $
--       (up . fromTree) t1
--       `shouldBe` Nothing
--
--     test "left, right, and up" $
--       (value . fromJust . right . fromJust . left . fromJust . up . fromJust . right . fromJust . up . fromJust . left . fromTree) t1
--       `shouldBe` 3
--
--     test "tree from deep focus" $
--       (toTree . fromJust . right . fromJust . left . fromTree) t1
--       `shouldBe` t1
--
--     test "setValue" $
--       (toTree . setValue 5 . fromJust . left . fromTree) t1
--       `shouldBe` t2
--
--     test "setValue after traversing up" $
--       (toTree . setValue 5 . fromJust . up . fromJust . right . fromJust . left . fromTree) t1
--       `shouldBe` t2
--
--     test "setLeft with Just" $
--       (toTree . setLeft (leaf 5) . fromJust . left . fromTree) t1
--       `shouldBe` t3
--
--     test "setRight with Nothing" $
--       (toTree . setRight Nothing . fromJust . left . fromTree) t1
--       `shouldBe` t4
--
--     test "setRight with subtree" $
--       (toTree . setRight (Just t5) . fromTree) t1
--       `shouldBe` t6
--
--     test "setValue on deep focus" $
--       (toTree . setValue 5 . fromJust . right . fromJust . left . fromTree) t1
--       `shouldBe` t7
--
--     test "different paths to same zipper" $
--       (right . fromJust . up . fromJust . left . fromTree) t1
--       `shouldBe` (right . fromTree) t1
