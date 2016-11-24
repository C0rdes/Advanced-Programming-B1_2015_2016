module SalsaInterp where
import Gpx
import SalsaAst
import Control.Applicative()
import Control.Monad

type Position = (Integer, Integer)

interpolate :: Integer -> Position -> Position -> [Position]
interpolate n pStart pEnd = let intervalX = fromIntegral (fst pEnd - fst pStart) / fromIntegral n
                                intervalY = fromIntegral (snd pEnd - snd pStart)/ fromIntegral n
                            in interpolateHelp intervalX intervalY (n-1) (fromIntegral(fst pEnd)) (fromIntegral(snd pEnd)) ++ [pEnd]
                            
interpolateHelp :: Double -> Double -> Integer -> Double -> Double -> [Position]
interpolateHelp _ _ 0 _ _ = []
interpolateHelp intx inty count lastx lasty = let nextx = round(lastx - intx)
                                                  nexty = round(lasty - inty)
                                              in interpolateHelp intx inty (count-1) (fromIntegral nextx) (fromIntegral nexty) ++ [(nextx, nexty)]

type Context = ([Shape], Animation, Integer)
          -- Rectangle Identifier BotLeftPosition Width Height Colour Visibility
data Shape = Rectangle Ident Position Integer Integer Colour Bool
          -- Circle Identifier Centerposition Radius Colour Visibility
           | Circle Ident Position Integer Colour Bool
     deriving (Show)  
     
     
newtype Salsa a = Salsa { runSalsa :: Context -> Either String (a, Context) }

instance Functor Salsa where
    fmap = liftM
    
instance Applicative Salsa where
    pure = return
    (<*>) = ap

instance Monad Salsa where
    -- return a :: a -> Salsa a)
    return a = Salsa $ \c -> Right (a, c) 
    -- (>>=) m f :: Salsa a -> (a -> Salsa b) -> Salsa b
    step >>= makeStep = Salsa $ \context -> 
        case runSalsa step context of
            Left err -> Left err
            Right (result, newContext) -> runSalsa (makeStep result) newContext
            
shapeGetPosition :: Shape -> Salsa Position
shapeGetPosition (Rectangle _ pos _ _ _ _) = return pos
shapeGetPosition (Circle _ pos _ _ _) = return pos

positionGetX :: Position -> Salsa Integer
positionGetX pos = return $ fst pos

positionGetY :: Position -> Salsa Integer
positionGetY pos = return $ snd pos

shapeGetVisibility :: Shape -> Salsa Bool
shapeGetVisibility (Rectangle _ _ _ _ _ vis) = return vis
shapeGetVisibility (Circle _ _ _ _ vis) = return vis

-- Gets the current context
getContext :: Salsa Context
getContext = Salsa $ \c -> Right (c, c)

setContext :: [Shape] -> Animation -> Integer -> Salsa ()
setContext shape animation n = Salsa $ \_ -> Right ((), (shape, animation, n))

initContext :: Integer -> Salsa ()
initContext n = Salsa $ \_ -> Right((), ([],[],n))                                
                                 
colourToString :: Colour -> String
colourToString col
    | col == Blue   = "Blue"
    | col == Plum   = "Plum"
    | col == Red    = "Red"
    | col == Green  = "Green"
    | col == Orange = "Orange"
    | otherwise     = "Black"

contextGetIdent :: [Shape] -> Ident -> Salsa Shape
contextGetIdent [] i = Salsa $ \_ -> Left $ "No shape with identifier " ++ i
contextGetIdent (x:xs) i = case x of
                        (Rectangle ident _ _ _ _ _) -> if i == ident then return x else contextGetIdent xs i
                        (Circle ident _ _ _ _) -> if i == ident then return x else contextGetIdent xs i

shapeToggleVisibility :: Shape -> Salsa Shape
shapeToggleVisibility (Rectangle ident pos width height colour visibility) = return $ Rectangle ident pos width height colour (not visibility)
shapeToggleVisibility (Circle ident pos radius colour visibility) = return $ Circle ident pos radius colour (not visibility)

contextRemoveShape :: [Shape] -> Int -> Ident -> Salsa [Shape]
contextRemoveShape [] _ i = Salsa $ \_ -> Left $ "No shape with identifier " ++ i
contextRemoveShape shapes 0 i = case head shapes of
                        (Rectangle ident _ _ _ _ _) -> if i == ident then return $ tail shapes else contextRemoveShape (tail shapes) 1 i 
                        (Circle ident _ _ _ _) -> if i == ident then return $ tail shapes else contextRemoveShape (tail shapes) 1 i
contextRemoveShape shapes n i = case head shapes of
                        (Rectangle ident _ _ _ _ _) -> if i == ident 
                                                 then 
                                                     let split = splitAt (n+1) shapes
                                                         valRemoved = init $ fst split
                                                     in return $ valRemoved ++ snd split
                                                 else contextRemoveShape shapes (n+1) i 
                        (Circle ident _ _ _ _) -> if i == ident 
                                                 then 
                                                     let split = splitAt (n+1) shapes
                                                         valRemoved = init $ fst split
                                                     in return $ valRemoved ++ snd split
                                                 else contextRemoveShape shapes (n+1) i 
 
contextAddShape :: [Shape] -> Shape -> Salsa [Shape]
contextAddShape shapes shape = return $ shape : shapes

saveContext :: Context -> Salsa Context
saveContext context = Salsa $ \_ -> Right (context, context)

checkShape :: [Shape] -> Ident -> Salsa ()
checkShape [] _ = return ()
checkShape (x:xs) i = case x of
                        (Rectangle ident _ _ _ _ _) -> if i == ident then Salsa $ \_ -> Left $ "Identifier " ++ i ++ " is already in use" else checkShape xs i
                        (Circle ident _ _ _ _) -> if i == ident then Salsa $ \_ -> Left $ "Identifier " ++ i ++ " is already in use" else checkShape xs i                                           

newRectangle :: Ident -> Integer -> Integer -> Integer -> Integer -> Colour -> Bool -> Salsa Shape
newRectangle ident x y w h col vis = return $ Rectangle ident (x, y) w h col vis                       
newCircle :: Ident -> Integer -> Integer -> Integer -> Colour -> Bool -> Salsa Shape
newCircle ident x y r col vis = return $ Circle ident (x, y) r col vis


checkEqualCommandIdent :: [Ident] -> [Ident] -> Salsa () 
checkEqualCommandIdent _ [] = return ()
checkEqualCommandIdent list (x:xs) = let lenlist = length $ filter (\val -> x == val) list 
                                     in 
                                        if lenlist > 1 
                                        then Salsa $ \_ -> Left "Attempt to modify same shape in parallel detected"
                                        else checkEqualCommandIdent list xs

getCommandIdent :: Command -> Ident
getCommandIdent (Rect ident _ _ _ _ _ _) = ident
getCommandIdent (Circ ident _ _ _ _ _ ) = ident
getCommandIdent (Move ident _) = ident
getCommandIdent (Toggle ident) = ident
getCommandIdent (Par _ _) = undefined -- Will never happen anyway
                                                                                
commandToIdentList :: Command -> [Ident]
commandToIdentList (Par command1 command2) = commandToIdentList command1 ++ commandToIdentList command2
commandToIdentList comm = [getCommandIdent comm]  

commandToIdentWrapper :: Command -> Salsa [Ident]
commandToIdentWrapper comm = Salsa $ \c -> Right (commandToIdentList comm, c)                                      

concatIdentList :: [Ident] -> [Ident] -> Salsa [Ident]
concatIdentList list1 list2 = Salsa $ \c -> Right (list1 ++ list2, c)

moveShape :: Bool -> Shape -> Integer -> Integer -> Salsa Shape
moveShape absVal (Rectangle ident (x0, y0) w h col vis) x y = 
                     if absVal 
                     then return $ Rectangle ident (x, y) w h col vis
                     else return $ Rectangle ident (x0+x, y0+y) w h col vis
moveShape absVal (Circle ident (x0, y0) r col vis) x y = 
                     if absVal 
                     then return $ Circle ident (x, y) r col vis
                     else return $ Circle ident (x0+x, y0+y) r col vis
                     
posToX :: Pos -> Salsa Integer
posToX (Abs expr1 _) = evalExpr expr1
posToX (Rel expr1 _) = evalExpr expr1

posToY :: Pos -> Salsa Integer
posToY (Rel _ expr2) = evalExpr expr2
posToY (Abs _ expr2) = evalExpr expr2

getAbs :: Pos -> Salsa Bool
getAbs (Abs _ _) = return True
getAbs (Rel _ _) = return False
                                             
 
salsaPlus :: Integer -> Integer -> Salsa Integer
salsaPlus e1 e2 = return $ e1 + e2
salsaMinus :: Integer -> Integer -> Salsa Integer
salsaMinus e1 e2 = return $ e1 - e2
salsaMult :: Integer -> Integer -> Salsa Integer
salsaMult e1 e2 = return $ e1 * e2
salsaDiv :: Integer -> Integer -> Salsa Integer
salsaDiv e1 e2 = return $ e1 `div` e2

evalExpr :: Expr -> Salsa Integer
evalExpr (Const integer) = Salsa $ \c -> Right (integer, c)
evalExpr (Plus expr1 expr2) = do
                             e1 <- evalExpr expr1
                             e2 <- evalExpr expr2
                             salsaPlus e1 e2
                             
evalExpr (Minus expr1 expr2) = do
                             e1 <- evalExpr expr1
                             e2 <- evalExpr expr2
                             salsaMinus e1 e2
                             
evalExpr (Mult expr1 expr2) = do
                             e1 <- evalExpr expr1
                             e2 <- evalExpr expr2
                             salsaMult e1 e2
evalExpr (Div expr1 expr2) = do
                             e1 <- evalExpr expr1
                             e2 <- evalExpr expr2
                             salsaDiv e1 e2
                             
evalExpr (Xproj ident) = do
                        shape <- evalGetShape ident
                        position <- shapeGetPosition shape
                        positionGetX position
                        
evalExpr (Yproj ident) = do
                        shape <- evalGetShape ident
                        position <- shapeGetPosition shape
                        positionGetY position

evalGetShape :: Ident -> Salsa Shape
evalGetShape ident = do
                 (shapes, _, _) <- getContext
                 contextGetIdent shapes ident
                        

-- updates the context, and shit.                        
command :: Command -> Salsa ()
-- Get context, translate command shape to 
command (Rect ident xStart yStart width height colour visible) = do
        (shapes, animation, n) <- getContext
        checkShape shapes ident
        x1 <- evalExpr xStart
        y1 <- evalExpr yStart
        w <- evalExpr width
        h <- evalExpr height
        newRect <- newRectangle ident x1 y1 w h colour visible
        newshapes <- contextAddShape shapes newRect
        setContext newshapes animation n
        
        
command (Circ ident xMid yMid radius colour visible) = do
        (shapes, animation, n) <- getContext
        checkShape shapes ident
        x1 <- evalExpr xMid
        y1 <- evalExpr yMid
        r <- evalExpr radius
        newCirc <- newCircle ident x1 y1 r colour visible
        newshapes <- contextAddShape shapes newCirc
        setContext newshapes animation n
        
command (Move ident position) = do
                            (shapes, animation, n) <- getContext
                            shape <- contextGetIdent shapes ident
                            absval <- getAbs position
                            x <- posToX position
                            y <- posToY position
                            newshape <- moveShape absval shape x y
                            newshapes <- remShapeAddNewShape ident shapes newshape
                            setContext newshapes animation n
                    
command (Toggle ident) = do
                     (shapes, animation, n) <- getContext              
                     shape <- contextGetIdent shapes ident                  
                     newshape <- shapeToggleVisibility shape            
                     newshapes <- remShapeAddNewShape ident shapes newshape     
                     setContext newshapes animation n               
                                                                 
command (Par command1 command2) = do
                             list1 <- commandToIdentWrapper command1
                             list2 <- commandToIdentWrapper command2
                             reslist <- concatIdentList list1 list2
                             _ <- checkEqualCommandIdent reslist reslist
                             _ <- command command1
                             command command2
                             
remShapeAddNewShape :: Ident -> [Shape] -> Shape -> Salsa [Shape]
remShapeAddNewShape ident shapelist shape = do
                                    remshape <- contextRemoveShape shapelist 0 ident
                                    contextAddShape remshape shape
                             
runProg :: Integer -> Program -> Either String Animation
runProg n program = case runSalsa (processCommands program) ([],[],n) of
                        Right (animation, _) -> Right animation
                        Left err -> Left err
                
processCommands :: Program -> Salsa Animation
processCommands [] = return []
processCommands (x:xs) = do
                     (oldshapes, _, _) <- getContext                          -- Get the old shapes
                     _ <- command x                                           -- run the first command
                     (newshapes, animation, n) <- getContext                  -- Get the new context
                     curanimation <- shapesToAnimation n oldshapes newshapes -- 
                     newanimation <- bindAnimations animation curanimation
                     _ <- setContext newshapes newanimation n
                     processCommands xs
                     
                     
shapesToAnimation :: Integer -> [Shape] -> [Shape] -> Salsa Animation
shapesToAnimation n oldshapes newshapes = do   
                                noMovement <- makeNoMovementList oldshapes newshapes
                                movement <- makeMovementList oldshapes newshapes
                                interpList <- interpolationList n oldshapes movement
                                anim1 <- noMoveAnimations n noMovement
                                anim2 <- moveAnimation movement interpList
                                zipAnimations anim1 anim2
                                        
noMoveAnimations :: Integer -> [Shape] -> Salsa Animation
noMoveAnimations _ [] = return []
noMoveAnimations n shapes = do
                     frame <- shapesToFrame shapes
                     frameNTimes n frame

moveAnimation :: [Shape] -> [[Position]] -> Salsa Animation
moveAnimation [] _ = return []
moveAnimation _ [] = return []
moveAnimation (x:xs) (y:ys) = do
                           
                     
interpolationList :: Integer -> [Shape] -> [Shape] -> Salsa [[Position]]
interpolationList _ _ [] = return []
interpolationList n oldshapes (x:xs) = do
                             ident <- getIdent x
                             oldshape <- getShape ident oldshapes
                             newpos <- shapeGetPosition x
                             oldpos <- shapeGetPosition oldshape
                             reslist <- interpolationList n oldshapes xs
                             tempinterpolation <- salsaInterpolate n oldpos newpos
                             addToPositionList tempinterpolation reslist
                             
salsaInterpolate :: Integer -> Position -> Position -> Salsa [Position]
salsaInterpolate n pos1 pos2 = return $ interpolate n pos1 pos2                             

addToPositionList :: [Position] -> [[Position]] -> Salsa [[Position]]
addToPositionList poslist poslistlist = return $ poslist : poslistlist

zipAnimations :: Animation -> Animation -> Salsa Animation
zipAnimations [] (_ : _) = Salsa $ \_ -> Left "Error in zipAnimations. Unequal list length"
zipAnimations (_ : _) [] = Salsa $ \_ -> Left "Error in zipAnimations. Unequal list length"
zipAnimations [] [] = return []
zipAnimations (x:xs) (y:ys) = do
                        anim <- zipAnimations xs ys
                        frame <- concatFrames x y
                        addFrameToAnimation frame anim
                        
concatFrames :: Frame -> Frame -> Salsa Frame
concatFrames frame1 frame2 = return $ frame1 ++ frame2
                     
frameNTimes :: Integer -> Frame -> Salsa Animation
frameNTimes 0 _ = return []
frameNTimes n frame = do
                 frames <- frameNTimes (n-1) frame
                 addFrameToAnimation frame frames
               
addFrameToAnimation :: Frame -> Animation -> Salsa Animation
addFrameToAnimation frame frames = return $ frame : frames
               

makeNoMovementList :: [Shape] -> [Shape] -> Salsa [Shape]
makeNoMovementList _ [] = do return []
makeNoMovementList oldshapes (x:xs) = do
                         isvisible <- shapeGetVisibility x
                         ident <- getIdent x
                         inoldshapes <- isInOldShapes oldshapes ident
                         oldshape <- getShape ident oldshapes
                         ismoved <- isMoved oldshape x
                         if isvisible 
                         then 
                            if inoldshapes && ismoved
                            then makeNoMovementList oldshapes xs
                            else do
                                 list <- makeNoMovementList oldshapes xs
                                 addShapeToList x list
                         else makeNoMovementList oldshapes xs

makeMovementList :: [Shape] -> [Shape] -> Salsa [Shape]
makeMovementList _ [] = do return []
makeMovementList oldshapes (x:xs) = do
                         isvisible <- shapeGetVisibility x
                         ident <- getIdent x
                         inoldshapes <- isInOldShapes oldshapes ident
                         oldshape <- getShape ident oldshapes
                         ismoved <- isMoved oldshape x
                         if isvisible 
                         then 
                            if inoldshapes && ismoved
                            then do
                                 list <- makeMovementList oldshapes xs
                                 addShapeToList x list
                            else makeMovementList oldshapes xs
                         else makeNoMovementList oldshapes xs
                         
                         
isMoved :: Shape -> Shape -> Salsa Bool
isMoved shape1 shape2 = do
                     pos1 <- shapeGetPosition shape1
                     pos2 <- shapeGetPosition shape2
                     posIsNotEqual pos1 pos2


posIsNotEqual :: Position -> Position -> Salsa Bool
posIsNotEqual pos1 pos2 = return $ pos1 == pos2
                     
addShapeToList :: Shape -> [Shape] -> Salsa [Shape]
addShapeToList shape shapes = return $ shape : shapes 
 
shapesToFrame :: [Shape] -> Salsa Frame
shapesToFrame [] = return []
shapesToFrame (x:xs) = do
                    gpxInstr <- shapetoGpxInstr x
                    frame <- shapesToFrame xs
                    
                    addgpxInstrToFrame gpxInstr frame
 
addgpxInstrToFrame :: GpxInstr -> Frame -> Salsa Frame
addgpxInstrToFrame gpx frame = return $ gpx : frame 
 
shapetoGpxInstr :: Shape -> Salsa GpxInstr
shapetoGpxInstr (Rectangle _ pos width height colour _) = return $ uncurry DrawRect pos width height $ colourToString colour
shapetoGpxInstr (Circle _ pos radius colour _) = return $uncurry DrawCirc pos radius $ colourToString colour

                                    
                                  
                                    
getIdent :: Shape -> Salsa Ident
getIdent (Rectangle ident _ _ _ _ _ ) = return ident
getIdent (Circle ident _ _ _ _) = return ident

isInOldShapes :: [Shape] -> Ident -> Salsa Bool
isInOldShapes [] _ = return False
isInOldShapes (x:xs) ident = case x of
                            (Rectangle i _ _ _ _ _) -> if ident == i then return True else isInOldShapes xs ident
                            (Circle i _ _ _ _) -> if ident == i then return True else isInOldShapes xs ident

bindAnimations :: Animation -> Animation -> Salsa Animation
bindAnimations anim1 anim2 = return $ anim1 ++ anim2
      
getShape :: Ident -> [Shape] -> Salsa Shape
getShape ident [] = Salsa $ \_ -> Left $ "No such shape with ident " ++ ident 
getShape ident1 (x:xs) = case x of
                        (Rectangle ident2 _ _ _ _ _) -> if ident1 == ident2 then return x else getShape ident1 xs
                        (Circle ident2 _ _ _ _) -> if ident1 == ident2 then return x else getShape ident1 xs
                                       