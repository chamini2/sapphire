{-# LANGUAGE LambdaCase #-}
{-|
    MIPS code generation module
 -}
module Language.Sapphire.MIPSGenerator
    ( MIPSGenerator
    , processMIPSGenerator
    ) where

import           Language.Sapphire.MIPS        as MIPS
import           Language.Sapphire.Program
import           Language.Sapphire.SappMonad   hiding (initialWriter)
import           Language.Sapphire.SymbolTable
import           Language.Sapphire.TAC         as TAC

import           Control.Monad                 (when)
import           Control.Monad.Reader          (asks)
import           Control.Monad.RWS             (RWS, execRWS)
import           Control.Monad.State           (gets, modify)
import           Control.Monad.Writer          (tell)
import           Data.Char                     (ord)
import           Data.Foldable                 (concat, forM_, mapM_, toList)
import qualified Data.Map.Strict               as Map (Map, adjust,
                                                       foldlWithKey, fromList,
                                                       lookup, member)
import           Data.Maybe                    (fromJust, fromMaybe, isJust)
import           Data.Sequence                 (Seq, empty, singleton)
import           Prelude                       hiding (EQ, GT, LT, concat,
                                                mapM_)

--------------------------------------------------------------------------------

type MIPSGenerator = RWS SappReader MIPSWriter MIPSState

--------------------------------------------------------------------------------
-- State

data MIPSState = MIPSState
    { table               :: SymbolTable
    , registerDescriptors :: RegDescriptorTable

    {-, variablesDescriptors :: -}
    }

type RegDescriptorTable = Map.Map Register RegDescriptor

initialRegDescriptors :: RegDescriptorTable
initialRegDescriptors = Map.fromList
    [ (T0, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (T1, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (T2, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (T3, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (T4, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (T5, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (T6, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (T7, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (T8, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (T9, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (S0, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (S1, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (S2, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (S3, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (S4, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (S5, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (S6, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (S7, RegDescriptor { values = [], genPurpose = True, dirty = False })
    {-, (A0, RegDescriptor { values = [], genPurpose = True, dirty = False })-}
    , (A1, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (A2, RegDescriptor { values = [], genPurpose = True, dirty = False })
    , (A3, RegDescriptor { values = [], genPurpose = True, dirty = False })
    {-, (V0, RegDescriptor { values = [], genPurpose = True, dirty = False })-}
    , (V1, RegDescriptor { values = [], genPurpose = True, dirty = False })
    ]

data RegDescriptor = RegDescriptor
    { values     :: [Location]
    , genPurpose :: Bool
    , dirty      :: Bool
    }

{-data VarDescriptor = VarDescriptor -}
    {-{ var       :: Location-}
    {-, locations :: Location-}
    {-}-}

----------------------------------------
-- Instances

instance SappState MIPSState where
    getTable       = table
    putTable tab s = s { table = tab }
    getStack   = undefined
    getScopeId = undefined
    getAst     = undefined
    putStack   = undefined
    putScopeId = undefined
    putAst     = undefined

instance Show MIPSState where
    show = showSappState

----------------------------------------
-- Initial

initialState :: MIPSState
initialState = MIPSState
    { table   = emptyTable

    , registerDescriptors = initialRegDescriptors
    {-, variablesDescriptors-}
    }

--------------------------------------------------------------------------------
-- Writer

type MIPSWriter = Seq MIPS.Instruction

----------------------------------------
-- Initial

initialWriter :: MIPSWriter
initialWriter = empty

----------------------------------------

generate :: MIPS.Instruction -> MIPSGenerator ()
generate = tell . singleton

--------------------------------------------------------------------------------
-- Building the Monad

buildMIPSGenerator :: SymbolTable -> Seq TAC -> MIPSGenerator ()
buildMIPSGenerator tab blocks = do
    modify $ \s -> s { table = tab }
    tell initialWriter
    emitPreamble

    let tac = concat $ fmap toList blocks
    flip mapM_ (filter (not . isComment) tac) $ \ins -> do
        generate $ MIPS.Comment $ show ins
        emit ins

--------------------------------------------------------------------------------
-- Using the Monad

processMIPSGenerator :: SappReader -> SymbolTable -> Seq TAC -> MIPSWriter
processMIPSGenerator r s = generateMIPS r . buildMIPSGenerator s

generateMIPS :: SappReader -> MIPSGenerator a -> MIPSWriter
generateMIPS r = snd . flip (flip execRWS r) initialState

emitPreamble :: MIPSGenerator ()
emitPreamble = do
    filePath <- asks file
    generate $ MIPS.Comment $ filePath ++ " - Sapphire compiler generated MIPS code"

    --  Generate .data section
    generateGlobals

    generate $ MIPS.PutDirective ".text"
    generate $ MIPS.PutDirective ".align 2"
    generate $ MIPS.PutDirective ".globl main"

{-
 -    Generates MIPS code for printing scalar data types
 -}
generatePrintString :: Label -> MIPSGenerator ()
generatePrintString lab = do
    generate $ Li V0 4
    generate $ La A0 (Label lab)
    generate Syscall


generatePrint :: Location -> MIPS.Code -> MIPSGenerator ()
generatePrint ref code  = do
    generate $ Li V0 code
    rSrc <- getRegister Read ref []
    generate $ Move A0 rSrc
    generate Syscall

generateRead :: Location -> MIPS.Code -> MIPSGenerator ()
generateRead ref code = do
    generate $ Li V0 code
    generate Syscall
    rDst <- getRegister Write ref []
    generate $ Move rDst V0

generateGlobals :: MIPSGenerator ()
generateGlobals = do
    generate $ MIPS.PutDirective ".data"
    syms <- gets (toSeq . getTable)
    forM_ syms $ \(idn, sym) -> do
        case sym of
            SymInfo { dataType = Lex String _ , offset = off } -> generate $ Asciiz ("_str" ++ show off) idn
            {-SymInfo { dataType = Lex Int    _ , offset = off } -> generate $ Word   ("_" ++ show off) idn-}
            _ -> return ()

----------------------------------------
--  Register allocation

{-|
 -  Given a Location, say r, of a current var or temporary,
 -  a reason (to read or to write), this function will assign
 -  a register for r according to the following rules:
 -
 -  - A register already holding r
 -  - An empty register
 -  - An unmodified register
 -  - A modified (dirty) register. We have to spill it
 -
 -}

data Reason = Read | Write
            deriving (Eq)

getRegister :: Reason -> Location -> [Register] -> MIPSGenerator Register
getRegister reason ref@(Address _ off isGlobal) avoid = do
    reg <- do
        mReg <- findRegisterWithContents ref
        case mReg of
            Just reg -> return reg
            Nothing -> do
                mEmptyReg <- findEmptyRegister
                reg <- case mEmptyReg of
                    Just emptyReg -> return emptyReg
                    Nothing -> do
                        reg <- selectRegisterToSpill avoid
                        spillRegister reg
                        return reg

                slaveLocation ref reg
                when (reason == Read) $ do
                    generate $ Lw reg (Indexed off (if isGlobal then GP else FP))
                    markClean reg
                return reg

    when (reason == Write) $ markDirty reg
    return reg

findRegisterWithContents :: Location -> MIPSGenerator (Maybe Register)
findRegisterWithContents ref = gets registerDescriptors >>= return . Map.foldlWithKey checkRegister Nothing
        where
            checkRegister Nothing      reg regDes = if ref `elem` values regDes then Just reg else Nothing
            checkRegister reg@(Just _) _   _    = reg

findEmptyRegister :: MIPSGenerator (Maybe Register)
findEmptyRegister = gets registerDescriptors >>= return . Map.foldlWithKey checkRegister Nothing
    where
        checkRegister mReg keyReg regDes = case (mReg, values regDes) of
            (Just _ , _ ) -> mReg
            (Nothing, []) -> Just keyReg
            (Nothing, _ ) -> Nothing

-- getRegDescriptor :: Register -> MIPSGenerator RegDescriptor
-- getRegDescriptor = flip getsRegDescriptor id

-- getsRegDescriptor :: Register -> (RegDescriptor -> a) -> MIPSGenerator a
-- getsRegDescriptor reg f = gets registerDescriptors >>= return . f . fromJust . Map.lookup reg

slaveLocation :: Location -> Register -> MIPSGenerator ()
slaveLocation ref reg = do
    tab <- gets registerDescriptors
    if Map.member reg tab
        then modify $ \s -> s { registerDescriptors = Map.adjust (\rd -> rd { values = ref : values rd }) reg tab }
        else error "MIPSGenerator.markDirty: marking non-existent register as dirty"

markDirty :: Register -> MIPSGenerator ()
markDirty reg = do
    tab <- gets registerDescriptors
    if Map.member reg tab
        then modify $ \s -> s { registerDescriptors = Map.adjust (\rd -> rd { dirty = True }) reg tab }
        else error "MIPSGenerator.markDirty: marking non-existent register as dirty"

markClean :: Register -> MIPSGenerator ()
markClean reg = do
    tab <- gets registerDescriptors
    if Map.member reg tab
        then modify $ \s -> s { registerDescriptors = Map.adjust (\rd -> rd { dirty = False }) reg tab }
        else error "MIPSGenerator.markDirty: marking non-existent register as dirty"

selectRegisterToSpill :: [Register] -> MIPSGenerator Register
selectRegisterToSpill avoid = gets registerDescriptors >>= \tab -> return . nonAvoid tab $ nonDirty tab
    where
        nonDirty :: RegDescriptorTable -> Maybe Register
        nonDirty = Map.foldlWithKey func Nothing
            where
                func mReg keyReg regDes = case (mReg, dirty regDes, keyReg `elem` avoid) of
                    (Just _ , _    , _    ) -> mReg
                    (Nothing, _    , True ) -> Nothing
                    (Nothing, True , _    ) -> Nothing
                    (Nothing, False, False) -> Just keyReg
        nonAvoid :: RegDescriptorTable -> Maybe Register -> Register
        nonAvoid tab = fromMaybe (fromJust $ Map.foldlWithKey func Nothing tab)
            where
                func mReg keyReg _ = case (mReg, keyReg `elem` avoid) of
                    (Just _ , _    ) -> mReg
                    (Nothing, True ) -> Nothing
                    (Nothing, False) -> Just keyReg

spillRegister :: Register -> MIPSGenerator ()
spillRegister reg = do
    tab <- gets registerDescriptors
    let regDes = fromJust $ Map.lookup reg tab
    when (dirty regDes) $ forM_ (values regDes) $ \(Address _ off isGlobal) ->
        generate $ Sw reg (Indexed off (pointerRegister isGlobal))
    modify $ \s -> s { registerDescriptors = Map.adjust (\rd -> rd { values = [] }) reg tab }

spillAllDirtyRegisters :: MIPSGenerator ()
spillAllDirtyRegisters = return ()

{-buildOperand :: Location -> MIPSGenerator Operand-}
{-buildOperand = \case-}
    {-Address   _ ref glob -> do-}
        {-off <- offset-}
        {-return $ Indexed off (if glob then GP else FP)-}
            {-where-}
                {-offset = case ref of-}
                    {-Constant val -> case val of-}
                        {-ValInt int -> return int-}
                        {-_          -> error "MIPSGenerator.buildOperand: constant is not an integer"-}
                    {-_            -> error "MIPSGenerator.buildOperand: offset is not a constant"-}

{-generateLoadConstant :: Location -> Int -> MIPSGenerator ()-}
{-generateLoadConstant dst imm = do-}
    {-reg <- getRegisterForWrite dst-}
    {-generate $ Li reg imm-}

{-generateLoadAddress :: Location -> Label -> MIPSGenerator ()-}
{-generateLoadAddress dst lab = do-}
    {-reg <- getRegisterForWrite dst-}
    {-generate $ La reg lab-}

----------------------------------------

emit :: TAC.Instruction -> MIPSGenerator ()
emit = \case

      TAC.PutLabel lab -> generate $ MIPS.PutLabel lab

      LoadConstant dst val -> do
        reg <- getRegister Write dst []
        let imm = case val of
                ValInt   v -> v
                ValFloat _ -> error "MIPSGenerator.emit.LoadConstant: loading float constant"
                ValBool  v -> if v then 1 else 0
                ValChar  v -> ord v
        generate $ Li reg imm

      Load dst b (Address _ off isGlobal) -> do
        rDst <- getRegister Write dst []
        generate $ Lw rDst (Indexed (b + off) (pointerRegister isGlobal))

      Store src b (Address _ off isGlobal) -> do
        rSrc <- getRegister Read src []
        generate $ Sw rSrc (Indexed (b + off) (pointerRegister isGlobal))

      BinaryOp x op y z -> do
        ry  <- getRegister Read y []
        rz  <- getRegister Read z [ry]
        rx  <- getRegister Write x [ry, rz]

        case op of
            ADD -> generate $ Add rx ry rz
            SUB -> generate $ Sub rx ry rz
            MUL -> generate $ Mul rx ry rz
            DIV -> generate (Div ry rz) >> generate (Mflo rx)
            MOD -> generate (Div ry rz) >> generate (Mfhi rx)
            POW -> error "'^' is not supported"
            OR  -> generate $ Or  rx ry rz
            AND -> generate $ And rx ry rz
            XOR -> generate $ Xor rx ry rz
            Rel rel -> case rel of
                EQ -> generate $ Seq rx ry rz
                NE -> generate $ Sne rx ry rz
                LT -> generate $ Slt rx ry rz
                LE -> generate $ Sle rx ry rz
                GT -> generate $ Sgt rx ry rz
                GE -> generate $ Sge rx ry rz

    -- Function related instructions
      BeginFunction w -> do
        generate $ Subu  SP SP (Const 8)            -- Decrement $sp to make space to save $ra, $fp
        {-generate $ Subu  SP SP (Const 12)           -- Decrement $sp to make space to save $ra, $fp and return value-}
        generate $ Sw    FP (Indexed 8 SP)          -- Save fp
        generate $ Sw    RA (Indexed 4 SP)          -- Save ra
                                                    -- Save return value
        generate $ Addiu FP SP (Const 8)            -- Setup new fp
        when (w /= 0) . generate $ Subu SP SP (Const w)    -- Decrement sp to make space for locals/temps

      EndFunction -> do
        return ()
        -- We could have an implicit return statement here, but in our case return statements are mandatory

      PushParam ref -> do
        generate $ Subu SP SP (Const 4)     -- Decrement sp to make space for param
        reg <- getRegister Read ref []
        generate $ Sw reg (Indexed 4 SP)    -- Copy param value to stack

      PopParams byts -> do
        generate $ Addi SP SP (Const byts)   -- Pop params of stack

      Return mayA -> do
        when (isJust mayA) $ getRegister Read (fromJust mayA) [] >>= generate . Move V0
        generate $ Move SP FP               -- Pop callee frame off stack
        {-generate $ Lw FP (Indexed (-8)  FP)    -- Restore saved return value-}
        generate $ Lw RA (Indexed (-4) FP)  -- Restore saved ra
        generate $ Lw FP (Indexed 0  FP)    -- Restore saved fp
        generate $ Jr RA                    -- Return from function

      PCall lab -> do
        generate $ Jal lab

      FCall lab addr -> do
        generate $ Jal lab
        -- get return value TO DO
        ret <- getRegister Write addr []
        generate $ Move ret V0  -- Copy return value from $v0 - MIPS Convention

      -- Print
      PrintInt    ref   -> generatePrint ref 1
      PrintFloat  ref   -> generatePrint ref 2
      PrintChar   ref   -> generatePrint ref 11
      PrintBool   ref   -> generatePrint ref 1
      PrintString lab   -> generatePrintString lab

      -- Read
      ReadInt   ref -> generateRead ref 5
      ReadFloat ref -> generateRead ref 6
      ReadChar  ref -> generateRead ref 12
      {-ReadBool  ref -> generateRead ref 1-}

      -- Goto
      Goto lab -> do
        spillAllDirtyRegisters
        generate $ B lab        --  Unconditional branch

      IfTrueGoto ref lab -> do
        reg <- getRegister Read ref []
        generate $ Bnez reg lab

      ins                      -> error $ "MIPSGenerator.emit unknown instruction " ++ show ins
