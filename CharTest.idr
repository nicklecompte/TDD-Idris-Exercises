isPercent : Char -> Bool
isPercent '%' = True
isPercent _ = False

data PercentView : Char -> Type where
    Percent  : PercentView '%'
    AnyOther : (c : Char) -> PercentView c



isPercent_Not_percent : (x : Char) -> Not (x = '%') -> isPercent x = False
isPercent_Not_percent '%' contra = absurd (contra Refl)
isPercent_Not_percent x contra = ?doesntwork

