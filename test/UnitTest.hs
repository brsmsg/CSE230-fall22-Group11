module UnitTest where
import Test.QuickCheck
import Linear.V2 (V2(..))
import Control.Lens (makeLenses, (^.), (.~), (%~), (&), _1, _2)
import Shaft

quickCheckN n = quickCheckWith (stdArgs {maxSuccess = n})

prop_time :: Depth -> Bool
prop_time d = case findMode d of
                        Just index -> (d >= modesOfTime !! index) && (index == 0 || (d < modesOfTime !! (index - 1)))
                        Nothing -> d < 0

prop_can_left :: [Int] -> Bool
prop_can_left xs = case shouldLeft' xs of
                            False -> xs==[]||(helper_left xs)==False
                            True -> (helper_left xs)==True
                    
prop_can_right :: [Int] -> Bool
prop_can_right xs = case shouldRight' xs of
                            False -> xs==[]||(helper_right xs)==False
                            True -> (helper_right xs)==True

helper_left :: [Int] -> Bool
helper_left [] = True
helper_left (x:xs) = (x>0) && (helper_left xs)

helper_right :: [Int] -> Bool
helper_right [] = False
helper_right (x:xs) = (x < gridWidth - 1) || (helper_right xs)

prop_on_spike_platform :: (Int, Int) -> [(Int, Int)] -> Bool
prop_on_spike_platform c cs = case inSpikePlatform' c' (cs', SpikePlatform) of
                                        True -> helper_platform c' cs' == True
                                        False -> helper_platform c' cs' == False
                                    where
                                        c' = (helper_int_2_cord c) 
                                        cs' = (helper_int_2_cord_list cs)

prop_on_normal_platform :: (Int, Int) -> [(Int, Int)] -> Bool
prop_on_normal_platform c cs = case inNormalPlatform' c' (cs', NormalPlatform) of
                                        True -> helper_platform c' cs' == True
                                        False -> helper_platform c' cs' == False
                                    where
                                        c' = (helper_int_2_cord c) 
                                        cs' = (helper_int_2_cord_list cs)

prop_on_left_platform :: (Int, Int) -> [(Int, Int)] -> Bool
prop_on_left_platform c cs = case inLeftPlatform' c' (cs', LeftPlatform) of
                                        True -> helper_platform c' cs' == True
                                        False -> helper_platform c' cs' == False
                                    where
                                        c' = (helper_int_2_cord c) 
                                        cs' = (helper_int_2_cord_list cs)
                                      
prop_on_right_platform :: (Int, Int) -> [(Int, Int)] -> Bool
prop_on_right_platform c cs = case inRightPlatform' c' (cs', RightPlatform) of
                                        True -> helper_platform c' cs' == True
                                        False -> helper_platform c' cs' == False
                                    where
                                        c' = (helper_int_2_cord c) 
                                        cs' = (helper_int_2_cord_list cs)

prop_on_heal_platform :: (Int, Int) -> [(Int, Int)] -> Bool
prop_on_heal_platform c cs = case inHealPlatform' c' (cs', HealPlatform) of
                                        True -> helper_platform c' cs' == True
                                        False -> helper_platform c' cs' == False
                                    where
                                        c' = (helper_int_2_cord c)
                                        cs' = (helper_int_2_cord_list cs)                                        

helper_platform :: Coord -> [Coord] -> Bool
helper_platform _ [] = False
helper_platform (c) (c':cs) = ((c^._1)==c'^._1 && c^._2==c'^._2)||helper_platform c (cs)

helper_int_2_cord :: (Int, Int) -> Coord
helper_int_2_cord (x, y) = V2 x y

helper_int_2_cord_list ::[(Int, Int)] -> [Coord]
helper_int_2_cord_list [] = []
helper_int_2_cord_list (c:cs) = (helper_int_2_cord c): (helper_int_2_cord_list cs)
