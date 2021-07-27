module Data.TSCompat where 

foreign import data Any :: Type
foreign import data OneOf :: Row Type -> Type
foreign import data StringConst :: Symbol -> Type
foreign import data OptionRecord :: Row Type -> Row Type -> Type
