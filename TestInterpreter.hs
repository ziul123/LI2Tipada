import AbsLI
import Interpreter

p1 = Prog [Fun Tvoid (Ident "main") [] [
        SDec $ Dec (TList Tint) $ Ident "array",
        SDec $ Dec Tbool $ Ident "is_null",
        SAss (Ident "is_null") $ Call (Ident "isNil") [EVar $ Ident "array"],
        SDec $ Dec Tbool $ Ident "not_null",
        SDec $ Dec Tint $ Ident "aux",
        SDec $ Dec (TList Tint) $ Ident "rest",
        SAss (Ident "array") $ ECons (EInt 4) $ ECons (EInt 3) $ ECons (EInt 2) ENil,
        SAss (Ident "not_null") $ ENot $ Call (Ident "isNil") [EVar $ Ident "array"],
        SAss (Ident "aux") $ Call (Ident "head") [EVar $ Ident "array"],
        SAss (Ident "rest") $ Call (Ident "tail") [EVar $ Ident "array"]
    ]]

p2 = Prog [Fun Tvoid (Ident "main") [] [
        SDec $ Dec (TPair Tint Tbool) $ Ident "pair",
        SDec $ Dec Tint $ Ident "f",
        SDec $ Dec Tbool $ Ident "s",
        SAss (Ident "pair") $ EPair (EInt 3) (ETrue),
        SAss (Ident "f") $ Call (Ident "fst") [EVar $ Ident "pair"],
        SAss (Ident "s") $ Call (Ident "snd") [EVar $ Ident "pair"]
    ]]

p3 = Prog [Fun Tvoid (Ident "main") [] [
        SDec $ Dec (TList (TPair Tint Tint)) $ Ident "lista",
        SDec $ Dec (TPair Tint Tint) $ Ident "aux",
        SAss (Ident "lista") $ ECons (EPair (EInt 3) (EInt 4)) $ ECons (EPair (EInt 1) (EInt 2)) ENil,
        SAss (Ident "aux") $ Call (Ident "head") [EVar $ Ident "lista"]
    ]]

p4 = Prog [Fun Tvoid (Ident "main") [] [
        SDec $ Dec (TList (TList Tint)) $ Ident "lista",
        SDec $ Dec (TList Tint) $ Ident "aux",
        SAss (Ident "lista") $ ECons (ECons (EInt 3) ENil) $ ECons (ECons (EInt 1) (ECons (EInt 2) ENil)) ENil,
        SAss (Ident "aux") $ Call (Ident "head") [EVar $ Ident "lista"]
    ]]

p5 = Prog [Fun Tvoid (Ident "main") [] [
        SDec $ Dec Tint $ Ident "a",
        SDec $ Dec Tint $ Ident "b",
        SAss (Ident "a") $ EInt 3,
        SAss (Ident "b") $ Call (Ident "inc") [EVar $ Ident "a"]
    ],
    Fun Tint (Ident "inc") [Dec Tint (Ident "x")] [
        SDec $ Dec Tint $ Ident "r",
        SAss (Ident "r") $ EAdd (EInt 1) $ EVar $ Ident "x",
        SReturn $ EVar $ Ident "r"
    ]]
