module Config exposing (Config, LabeledItem, configurations, default, stringValue)

import Unit.Money as Money exposing (Money)
import Unit.Unit as Unit exposing (Unit, UnitCost)


type alias Config =
    { name : String
    , unitCost : UnitCost
    , unitPrice : UnitCost
    , stockOnHandThreshold : Unit
    , lowOrder : Unit
    , highOrder : Unit
    , ccOrderMax : Money
    , initialStock : Unit
    , initialFiatBalance : Money
    , initialCCBalance : Money
    }


default : Config
default =
    { name = "Default"
    , unitCost = Unit.unitCost 1.0
    , unitPrice = Unit.unitCost 2.0
    , stockOnHandThreshold = Unit.create 10
    , lowOrder = Unit.create 5
    , highOrder = Unit.create 20
    , ccOrderMax = Money.create 5
    , initialStock = Unit.create 10
    , initialFiatBalance = Money.create 0
    , initialCCBalance = Money.create 0
    }


configurations =
    [ default, config1, config2 ]


config1 =
    { default | name = "Initial Fiat Balance 8", initialFiatBalance = Money.create 8 }


config2 =
    { default | name = "Initial CC Balance 8", initialCCBalance = Money.create 8 }


type alias LabeledItem =
    { label : String, value : String }


stringValue : Config -> List LabeledItem
stringValue data =
    [ { label = "Configuration", value = data.name }
    , { label = "Stock", value = Unit.stringVal data.initialStock }
    , { label = "Fiat balance", value = Money.stringVal data.initialFiatBalance }
    , { label = "CC balance", value = Money.stringVal data.initialCCBalance }
    ]



--
--
--config1 =
--    { name = "Basic"
--    , initialStock = Unit.create 10
--    , initialFiatBalance = Money.create 0
--    , initialCCBalance = Money.create 0
--    }
--
--
--config2 =
--    { name = "With CC"
--    , initialStock = Unit.create 10
--    , initialFiatBalance = Money.create 0
--    , initialCCBalance = Money.create 0
--    }
