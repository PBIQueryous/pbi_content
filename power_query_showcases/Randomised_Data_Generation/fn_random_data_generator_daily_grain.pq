let
  customFunction =

    let
     // 1.1: Edit function metadata here
      documentation = [
        Documentation.Name = " fnRandomizedData (Daily Granularity) ", 
        Documentation.Description = " Generate random dataset with Category, Subcategory, Date fields, and variances. ",
        Documentation.LongDescription = "    
                <p><b>  fnRandomizedData  </b></p>
                <li>------------------------------------------------------</li>
                <li><b>  Creator: </b> Imran Haq  </li>
                <li><b>  Web: </b> https://github.com/PBIQueryous/  </li>
                <li><b>  Acknowledgements: </b> PBIQueryous  </li>
                <li><b>  LinkedIn: </b> (https://www.linkedin.com/in/imran-haq)  </li>
                <li>------------------------------------------------------</li>
                <p><b>  Function Description:  </b></p>
                <p>  This function generates a dataset based on categories, subcategories, and random sales/plan quantities for a dynamic date period two years in the past & future.  </p>
                <p><b>  Returns:  </b></p>
                <p>  A table of random data.  </p>
            ",
        Documentation.Category = " Random Data Generation ", 
        Documentation.Source = " PBI Queryous ", 
        Documentation.Version = " 1.0 ", 
        Documentation.Author = " Imran Haq ", 
        Documentation.Examples = {
          [
            Description = " Generate a  ",
            Code = " = fnRandomizedData() ",
            Result = 
"
  1. Categories/Subcategories
  2. Randomized Data (Sales, Plan, Budget, etc.)
  3. Date ranges from 2 years before the current year to 2 years after
"
          ]
        }
      ],

      // Function logic incorporating dynamic date table generation and random data creation
      invokeFunction = () =>
      let
        CategoriesTable = Table.FromRecords({
            [CategoryID=1, Category="Laptops"], 
            [CategoryID=2, Category="PCs"],
            [CategoryID=3, Category="Tablets"], 
            [CategoryID=4, Category="Phones"]
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
            dayList = List.Dates(startYear, Duration.Days(endYear - startYear) + 1, #duration(1, 0, 0, 0)),
            build_Table = Table.FromList(dayList, Splitter.SplitByNothing(), {"Date"}),
            add_Records = Table.AddColumn(build_Table, "_Table", each [
                Year = Date.Year([Date]), 
                Month = Date.MonthName([Date]), Day = Date.Day([Date]), 
                DayOfWeek = Date.DayOfWeekName([Date]), 
                MonthYear_TXT = Date.ToText([Date], "MMM-yy"), 
                Date_TXT = Date.ToText([Date], "dd-MMM-yyyy"), 
                MonthYear_INT = Year * 100 + Date.Month([Date]), 
                MonthStart = Date.StartOfMonth([Date]), 
                MonthEnd = Date.EndOfMonth([Date]),
                isYTD_INT = if [Date] <= Date.From(DateTime.FixedLocalNow()) then 1 else 0, 
                isYTD_BOOL = if [Date] <= Date.From(DateTime.FixedLocalNow()) then true else false
            ]),
            expandColumns = Table.ExpandRecordColumn(add_Records, "_Table", Record.FieldNames(Record.Combine(add_Records[_Table])))
        in expandColumns,

        current_year_start = Date.AddYears(Date.StartOfYear(Date.From(DateTime.FixedLocalNow())), 0),
        tbl_nested_records = 
          Table.AddColumn(MergedTable, "DateRecords", 
            each fx_insert_dates(Date.Year(Date.AddYears(current_year_start, -2)), 
            Date.Year(Date.AddYears(current_year_start, 2))), 
            type table [Year = Int64.Type, Date = date, MonthStart = date, MonthEnd = date, MonthYear_TXT = text]),
        exp_nested_records = Table.ExpandTableColumn(tbl_nested_records, "DateRecords", {"Year", "Date", "MonthStart", "MonthEnd", "MonthYear_TXT"}),

        max_quantity = 20,
        records_values = Table.AddColumn(exp_nested_records, "records", each [
            SalesQuantity = Number.RoundTowardZero(Number.RandomBetween(0, max_quantity)),
            PlanQuantity = Number.RoundTowardZero(Number.RandomBetween(0, max_quantity * 1.3)),
            BudgetQuantity = Number.RoundTowardZero(Number.RandomBetween(5, max_quantity * 1.5)),
            AC = [UnitPrice] * SalesQuantity, PL = [UnitPrice] * PlanQuantity, BU = [UnitPrice] * BudgetQuantity,
            vsPL = AC - PL, vsBU = AC - BU
        ], type [SalesQuantity = number, PlanQuantity = number, BudgetQuantity = number, AC = number, PL = number, BU = number, vsPL = number, vsBU = number]),

        exp_records_values = Table.ExpandRecordColumn(records_values, "records", Record.FieldNames(records_values[records]{0}))
      in exp_records_values
    ,

    // 2.0: Change parameter metadata here
    fnType = type function (
        // No input parameters required for this version of the function
        // Metadata here is optional as it doesn't take input for the basic dataset generation
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