let
  customFunction =

    let
     // 1.1: Edit function metadata here
      documentation = [
        Documentation.Name = " fnMonthlyRandomizedData (Monthly Granularity) ", 
        Documentation.Description = " Generate random dataset with Category, Subcategory, Monthly Date fields, and variances. ",
        Documentation.LongDescription = "    
                <p><b>  fnMonthlyRandomizedData  </b></p>

                <li>------------------------------------------------------</li>
                <li><b>  Creator: </b> Imran Haq  </li>
                <li><b>  Web: </b> https://github.com/PBIQueryous/  </li>
                <li><b>  Acknowledgements: </b> PBIQueryous  </li>
                <li><b>  LinkedIn: </b> (https://www.linkedin.com/in/imran-haq)  </li>
                <li>------------------------------------------------------</li>
                <p><b>  Function Description:  </b></p>
                <p>  This function generates a dataset based on categories, subcategories, and random sales/plan quantities for monthly dates from the start year to 2 years in the future.  </p>
                <p><b>  Returns:  </b></p>
                <p>  A table of random data with monthly granularity.  </p>
            ",
        Documentation.Category = " Random Data Generation ", 
        Documentation.Source = " PBI Queryous ", 
        Documentation.Version = " 1.0 ", 
        Documentation.Author = " Imran Haq ", 
        Documentation.Examples = {
          [
            Description = " Generate dataset with random quantities (Monthly) ",
            Code = " = fnMonthlyRandomizedData() ",
            Result = 
"
  1. Categories/Subcategories
  2. Randomized Data (Sales, Plan, Budget, etc.)
  3. Monthly Date ranges from 2 years before the current year to 2 years after
"
          ]
        }
      ],

      // Function logic incorporating dynamic date table generation and random data creation
      invokeFunction = () =>
      let
        CategoriesTable = Table.FromRecords({
            [CategoryID=1, Category="Laptops"], [CategoryID=2, Category="PCs"],
            [CategoryID=3, Category="Tablets"], [CategoryID=4, Category="Phones"]
        }, type table [CategoryID = Int64.Type, Category = Text.Type]),

        SubcategoriesTable = Table.FromRecords({
            [CategoryID=1, Subcategory="Gaming Laptops", UnitPrice=1200.00], 
            [CategoryID=1, Subcategory="Business Laptops", UnitPrice=1000.00], 
            [CategoryID=1, Subcategory="Ultrabooks", UnitPrice=1300.00], 
            [CategoryID=2, Subcategory="Gaming PCs", UnitPrice=1500.00], 
            [CategoryID=2, Subcategory="Workstations", UnitPrice=2000.00], 
            [CategoryID=2, Subcategory="Mini PCs", UnitPrice=900.00], 
            [CategoryID=3, Subcategory="Android Tablets", UnitPrice=400.00], 
            [CategoryID=3, Subcategory="iOS Tablets", UnitPrice=600.00], 
            [CategoryID=3, Subcategory="Windows Tablets", UnitPrice=450.00], 
            [CategoryID=4, Subcategory="Smartphones", UnitPrice=700.00], 
            [CategoryID=4, Subcategory="Feature Phones", UnitPrice=150.00], 
            [CategoryID=4, Subcategory="Phablets", UnitPrice=500.00]
        }, type table [CategoryID = Int64.Type, Subcategory = Text.Type, UnitPrice = Currency.Type]),

        MergedTable = Table.Join(CategoriesTable, "CategoryID", SubcategoriesTable, "CategoryID"),

        fx_insert_dates = (Start_Year as number, End_Year as number) =>
        let
            startYear = #date(Start_Year, 1, 1),
            endYear = #date(End_Year, 12, 31),
            monthList = List.Generate(() => startYear, each _ <= endYear, each Date.AddMonths(_, 1)),
            build_Table = Table.FromList(monthList, Splitter.SplitByNothing(), {"Date"}),
            add_Records = Table.AddColumn(build_Table, "_Table", each [
                Year = Date.Year([Date]), 
                Month = Date.MonthName([Date]), 
                Month_NUM = Date.Month([Date]), 
                MonthYear_TXT = Date.ToText([Date], "MMM-yy"), 
                EndOfMonth = Date.EndOfMonth([Date]), 
                StartOfMonth = Date.StartOfMonth([Date]), 
                MonthYear_INT = Year * 100 + Date.Month([Date]), 
                isYTD_INT = if [EndOfMonth] <= Date.From(DateTime.FixedLocalNow()) then 1 else 0, 
                isYTD_BOOL = if [EndOfMonth] <= Date.From(DateTime.FixedLocalNow()) then true else false
            ]),
            expandColumns = 
              Table.ExpandRecordColumn(add_Records, "_Table", Record.FieldNames(Record.Combine(add_Records[_Table])))
        in expandColumns,

        current_year_start = Date.AddYears(Date.StartOfYear(Date.From(DateTime.FixedLocalNow())), 0),
        tbl_nested_records = 
          Table.AddColumn(MergedTable, "DateRecords", 
            each fx_insert_dates(Date.Year(Date.AddYears(current_year_start, -2)), 
            Date.Year(Date.AddYears(current_year_start, 2))), 
            type table [Year = Int64.Type, Date = date, EndOfMonth = date]),
        exp_nested_records = Table.ExpandTableColumn(tbl_nested_records, "DateRecords", {"Year", "Date", "EndOfMonth"}),

        max_quantity = 200,
        records_values = Table.AddColumn(exp_nested_records, "records", each [
            SalesQuantity = Number.RoundTowardZero(Number.RandomBetween(50, max_quantity)),
            PlanQuantity = Number.RoundTowardZero(Number.RandomBetween(100, max_quantity * 0.8)),
            BudgetQuantity = Number.RoundTowardZero(Number.RandomBetween(50, max_quantity * 1.6)),
            AC = [UnitPrice] * SalesQuantity, PL = [UnitPrice] * PlanQuantity, BU = [UnitPrice] * BudgetQuantity,
            vsPL = AC - PL, vsBU = AC - BU
        ], type [SalesQuantity = number, PlanQuantity = number, BudgetQuantity = number, AC = number, PL = number, BU = number, vsPL = number, vsBU = number]),

        exp_records_values = Table.ExpandRecordColumn(records_values, "records", Record.FieldNames(records_values[records]{0}))
      in exp_records_values
    ,

    // 2.0: Change parameter metadata here
    fnType = type function (
        // No input parameters required for this version of the function
      ) as table,

    // 5.0: Choose between Parameter Documentation or Function Documentation
    functionDocumentation =  // -- function metadata                       
    Value.ReplaceType(invokeFunction, Value.ReplaceMetadata(Value.Type(invokeFunction), documentation)), 

    parameterDocumentation =  // -- parameter metadata                        
    Value.ReplaceType(functionDocumentation, fnType)

    in
      functionDocumentation
in
  customFunction