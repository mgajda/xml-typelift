-- | Just a list of files we perform our testing on.
module TestSet(testFiles) where

testFiles :: [String]
testFiles = ["test/data/person.xsd"
            ,"test/data/simple.xsd"
            ,"test/data/test.xsd"
            --,"test/shiporder.xsd"
            ,"test/data/customersOrders.xsd"
            --,"test/address.xsd"
            --,"test/shiporder.divided.xsd"
            --,"test/cd-list.xsd"
            ,"test/data/shiporder.named.xsd"
            --,"test/XMLSchema-cleaned.xsd"
            ,"test/data/contactExample.xsd"
            --,"test/purchaseOrder.xsd"
            --,"test/XMLSchema.xsd"
            ,"test/data/contacts.xsd"
            --,"test/recipe.xsd"
            --,"../tuxml/tuxml_schema-883.xsd"
            ]

