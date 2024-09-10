let
  customFunction =  // fn_CustomPeriodCalendar                                                            
  /* ------------------------------ 
  Author: Imran Haq - PBI QUERYOUS
  Description: fn_CustomPeriodCalendar
  Special Thanks: SQLBI (Alberto Ferrari and Marco Russo)
  Link: https://www.sqlbi.com/
 ---------------------------------*/
// NOTE: Requires an Key on the FACT Table: 
// OriginalID = Date.Year([Date]) * MonthsInYear + Date.Month([Date]) - 1
// or ActualYearMonthID = YYYYMM (202314)

  // 1.0: invoke function & define parameter inputs
  
    let
      invokeFunction = (
        minYear as number, 
        maxYear as number, 
        MonthsInYear as number, 
        optional FYMonthNUM as number
      ) as table =>
        // ------------------------------------------------------------------
        // 2.0: function transformations
        let
        //   // // //Parameters
        // //   StartDate = #date(2023, 1, 1),
        // //   EndDate = #date(2024, 12, 31),
        //   minYear = 2021,
        //   maxYear = 2023,
        //   MonthsInYear = 14,
        //   FYMonthNUM = null,
          //----------------------------------------------------------\\      
          //Date table code                 
  FYMonthStart = List.Select({1 .. 12}, each _ = FYMonthNUM){0}? ?? 1,
  StartDate = Date.From("01/" & Text.From(FYMonthStart) & "/" & Text.From(minYear)),
  EndDate = let
monthAdjustment = FYMonthStart + (MonthsInYear - 12) - 1,
monthNumber = if MonthsInYear > 12 then monthAdjustment else MonthsInYear,
calc = Date.EndOfMonth( Date.From("1/" & Text.From(monthNumber) & "/" & Text.From(maxYear)) )
in calc,
  StartYear = minYear,
  StartMonth = Date.Month(StartDate),
  EndYear = maxYear,
  EndMonth = Date.Month(EndDate),
  var_CurrentYear = Date.Year(Date.From(DateTime.FixedLocalNow())),
  var_CurrentMonth = Date.Month(Date.From(DateTime.FixedLocalNow())),
  var_CurrentDate = Date.From(DateTime.FixedLocalNow()),
  var_CountDays = Duration.Days(Duration.From(EndDate - StartDate)) + 1,
  list_Dates = List.Dates(StartDate, var_CountDays, #duration(1, 0, 0, 0)),
  toTbl_Dates = Table.TransformColumnTypes(
    Table.RenameColumns(
      Table.FromList(list_Dates, Splitter.SplitByNothing(), null, null, ExtraValues.Error),
      {{"Column1", "Date"}}
    ),
    {{"Date", type date}}
  ),
  col_MonthEnd = Table.AddColumn(toTbl_Dates, "MonthEnd", each Date.EndOfMonth([Date]), type date),
  col_DataGrainOG = Table.TransformColumnTypes(
    Table.AddColumn(
      col_MonthEnd,
      "OG_MonthYear_ID",
      each Date.Year([Date]) * MonthsInYear + Date.Month([Date]) - 1
    ),
    {{"OG_MonthYear_ID", Int64.Type}}
  ),
  tempTbl_Grouped =
    let
      _record = [
        MonthEnd = Table.RemoveColumns(
          Table.Group(col_DataGrainOG, {"MonthEnd"}, {{"Detail", each _}}),
          {"Detail"}
        )
      ]
    in
      _record,
  ListMin =
    let
      result = [
        xStartYear  = StartYear,
        xMonths     = MonthsInYear,
        xStartMonth = StartMonth,
        xCalc1      = if (xStartMonth < FYMonthStart) then 1 else 0,
        xCalc2      = (xStartYear * xMonths + xStartMonth) - 1,
        xCalc3      = (xMonths - 12) * xCalc1,
        xResult     = xCalc2 - xCalc3
      ]
    in
      result[xResult],
  ListMax =
    let
      result = [
        xEndYear  = EndYear,
        xMonths   = MonthsInYear,
        xEndMonth = EndMonth,
        xCalc1    = if (xEndMonth < FYMonthStart) then 1 else 0,
        xCalc2    = (xEndYear * xMonths + xEndMonth) - 1,
        xCalc3    = (xMonths - 12) * xCalc1,
        xResult   = xCalc2 - xCalc3
      ]
    in
      result[xResult],
  GenerateList =
    if MonthsInYear > 12 then
      {ListMin .. ListMax}
    else
      {
        List.Min(col_DataGrainOG[OG_MonthYear_ID]) .. List.Max(
          col_DataGrainOG[OG_MonthYear_ID]
        )
      },
  tbl_MonthlyGrain = Table.TransformColumnTypes(Table.FromList(
    GenerateList,
    Splitter.SplitByNothing(),
    {"OG_MonthYear_ID"},
    null,
    ExtraValues.Error
  ), {{"OG_MonthYear_ID", Int64.Type}}),
  tbl_choose = if MonthsInYear > 12 then tbl_MonthlyGrain else col_DataGrainOG,
  col_FiscalMonth = Table.AddColumn(
    tbl_choose,
    "FiscalMonth_NUM",
    each
      let
        result = [
          xMonthInt = [OG_MonthYear_ID] + 1,
          xCond1    = if (FYMonthStart > 1) then 1 else 0,
          xCalc1    = MonthsInYear + 1 - FYMonthStart,
          xMod      = [OG_MonthYear_ID] + (1 * xCond1 * xCalc1),
          xResult   = Number.Mod(xMod, MonthsInYear) + 1
        ]
      in
        result[xResult],
    Int64.Type
  ),
  fn_GetMonth =
    let
      invokeFunction = (fiscalPeriod as any, fiscalStart as number) =>
        let
          fiscalMonth = if fiscalStart = null then 1 else fiscalStart,
          columnName  = fiscalPeriod,
          calc        = (12 - fiscalMonth) + 1,
          x           = Number.Abs(columnName + fiscalMonth) - 1,
          y           = Number.Abs(columnName + fiscalMonth) - 13,
          z           = if columnName <= calc then x else y
        in
          z
    in
      invokeFunction,
  col_GetActualMonth = Table.AddColumn(
    col_FiscalMonth,
    "ActualMonth_NUM",
    each fn_GetMonth([FiscalMonth_NUM], FYMonthStart),
    Int64.Type
  ),
  col_FiscalYearNUM = Table.AddColumn(
    col_GetActualMonth,
    "FiscalYear_NUM",
    each
      let
        result = [
          xMonthInt = [OG_MonthYear_ID],
          xCond1    = if (FYMonthStart > 1) then 1 else 0,
          xCalc1    = MonthsInYear + 1 - FYMonthStart,
          xMod      = xMonthInt + (1 * xCond1 * xCalc1),
          xResult   = Number.IntegerDivide(xMod, MonthsInYear) - 1
        ]
      in
        result[xResult],
    Int64.Type
  ),
  col_YearNUM = Table.AddColumn(
    col_FiscalYearNUM,
    "ActualYear_NUM",
    each
      let
        x1 = if [ActualMonth_NUM] > [FiscalMonth_NUM] then 1 else 0,
        x2 = ([FiscalYear_NUM] - 1 * x1) + 1
      in
        x2,
    Int64.Type
  ),
  var_FiscalMonthOFFSET = MonthsInYear + 1 - (MonthsInYear - 12),
  col_YearMonthID = Table.AddColumn(
    col_YearNUM,
    "ActualYearMonth_ID",
    each
      let
        cols = [
          cond1          = [FiscalMonth_NUM] <= 12 and FYMonthStart > 1,
          calc1          = [FiscalMonth_NUM] + FYMonthStart,
          calc2          = var_FiscalMonthOFFSET - FYMonthStart,
          cond2          = if [FiscalMonth_NUM] > calc2 then var_FiscalMonthOFFSET else 1,
          calc3          = (calc1 - cond2),
          MonthID        = if cond1 then calc3 else [FiscalMonth_NUM],
          condX          = if MonthID > [FiscalMonth_NUM] then 1 else 0,
          ActualYear_NUM = [FiscalYear_NUM] * condX,
          YearMonth_ID   = [ActualYear_NUM] * 100 + MonthID
        ]
      in
        cols[YearMonth_ID],
    Int64.Type
  ),
  col_FiscalYearMonthID = Table.AddColumn(
    col_YearMonthID,
    "FiscalYearMonth_ID",
    each [FiscalYear_NUM] * 100 + [FiscalMonth_NUM],
    Int64.Type
  ),
  col_CalendarMonth = Table.AddColumn(
    col_FiscalYearMonthID,
    "RelativeMonth_NUM",
    each if [FiscalMonth_NUM] >= 12 then 7 else [ActualMonth_NUM],
    Int64.Type
  ),
  col_Month_Actual = Table.AddColumn(
    col_CalendarMonth,
    "Actual_Date",
    each Date.From("1/" & Text.From([ActualMonth_NUM]) & "/" & Text.From([ActualYear_NUM])),
    type date
  ),
  col_Month_Relative = Table.AddColumn(
    col_Month_Actual,
    "Relative_Date",
    each Date.From("1/" & Text.From([RelativeMonth_NUM]) & "/" & Text.From([ActualYear_NUM])),
    type date
  ),
  col_ActualMonthEnd = Table.AddColumn(
    col_Month_Relative,
    "ActualMonth_End",
    each Date.EndOfMonth([Actual_Date]),
    type date
  ),
  col_RelativeMonthEnd = Table.AddColumn(
    col_ActualMonthEnd,
    "RelativeMonth_End",
    each Date.EndOfMonth([Relative_Date]),
    type date
  ),
  col_Return = Table.AddColumn(
    col_RelativeMonthEnd,
    "Return",
    each "R" & Text.PadStart(Text.From([FiscalMonth_NUM]), 2, "0"),
    type text
  ),
  col_ActualMonthName = Table.AddColumn(
    col_Return,
    "ActualMonth_Name",
    each Text.Start(Date.MonthName([ActualMonth_End]), 3),
    type text
  ),
  col_RelativeMonthName = Table.AddColumn(
    col_ActualMonthName,
    "RelativeMonth_Name",
    each Text.Start(Date.MonthName([RelativeMonth_End]), 3),
    type text
  ),
  col_MonthPeriod = Table.AddColumn(
    col_RelativeMonthName,
    "Month Period",
    each
      let
        x0 = Text.PadStart(Text.From([FiscalMonth_NUM]), 2, "0"),
        x1 = if [FiscalMonth_NUM] <= 12 then [RelativeMonth_Name] else "P" & x0
      in
        x1,
    type text
  ),
  col_FiscalPeriod = Table.AddColumn(
    col_MonthPeriod,
    "Fiscal Period",
    each
      let
        StartYear = Text.End(Text.From([FiscalYear_NUM]), 2),
        EndYear   = Text.End(Text.From([FiscalYear_NUM] + 1), 2)
      in
        StartYear & "/" & EndYear,
    type text
  ),
  col_Quarter = Table.AddColumn(
    col_FiscalPeriod,
    "Quarter",
    each "Q" & Text.From(Date.QuarterOfYear([Actual_Date])),
    type text
  ),
  col_FiscalQuarter = Table.AddColumn(
    col_Quarter,
    "Fiscal Quarter",
    each
      let
        xCol   = [FiscalMonth_NUM],
        x1     = xCol >= 1 and xCol <= 3,
        x2     = xCol >= 4 and xCol <= 6,
        x3     = xCol >= 7 and xCol <= 9,
        x4     = xCol >= 10 and xCol <= 12,
        result = if x1 then "FQ1" else if x2 then "FQ2" else if x3 then "FQ3" else "FQ4"
      in
        result,
    type text
  ),
  col_MonthOFFSET = Table.AddColumn(
    col_FiscalQuarter,
    "MonthOFFSET",
    each ((12 * Date.Year([Relative_Date])) + Date.Month([Relative_Date]))
      - ((12 * Date.Year(Date.From(var_CurrentDate))) + Date.Month(Date.From(var_CurrentDate))),
    type number
  ),
  col_FiscalYearOFFSET = Table.AddColumn(
    col_MonthOFFSET,
    "FiscalYearOFFSET",
    each try
      (if [RelativeMonth_NUM] >= FYMonthStart then [ActualYear_NUM] + 1 else [ActualYear_NUM])
        - (if var_CurrentMonth >= FYMonthStart then var_CurrentYear + 1 else var_CurrentYear)
    otherwise
      null,
    type number
  ),
  fn_FiscalQuarterOFFSET =
    let
      fxAddFiscalQuarterOffset = (Date as date, FiscalYearStartMonth as number) as number =>
        let
          CurrentDate = Date.From(DateTime.LocalNow()),
          n =
            if List.Contains({1 .. 12}, FiscalYearStartMonth) and FiscalYearStartMonth > 1 then
              FiscalYearStartMonth - 1
            else
              0,
          FiscalQuarterOffset = (
            (4 * Date.Year(Date.AddMonths(Date.StartOfMonth(Date), - n)))
              + Date.QuarterOfYear(Date.AddMonths(Date.StartOfMonth(Date), - n))
          )
            - (
              (4 * Date.Year(Date.AddMonths(Date.StartOfMonth(CurrentDate), - n)))
                + Date.QuarterOfYear(Date.AddMonths(Date.StartOfMonth(CurrentDate), - n))
            )
        in
          FiscalQuarterOffset
    in
      fxAddFiscalQuarterOffset,
  col_FiscalQuarterOFFSET = Table.AddColumn(
    col_FiscalYearOFFSET,
    "FiscalQuarterOFFSET",
    each fn_FiscalQuarterOFFSET([Actual_Date], FYMonthStart),
    Int64.Type
  ),
  col_FiscalQuarterYear = Table.AddColumn(
    col_FiscalQuarterOFFSET,
    "Fiscal Quarter & Year",
    each "FQ"
      & Text.From(
        Number.RoundUp(Date.Month(Date.AddMonths([Relative_Date], - (FYMonthStart - 1))) / 3)
      )
      & " "
      & (
        if [RelativeMonth_NUM] >= FYMonthStart and FYMonthStart > 1 then
          Text.End(Text.From([ActualYear_NUM] + 0), 2)
        else
          Text.End(Text.From([ActualYear_NUM] - 1), 2)
      ),
    type text
  ),
  col_PeriodReturn = Table.AddColumn(col_FiscalQuarterYear, "Period Return", each [Fiscal Period] & ": " & [Return] & " - " & Date.ToText( [Actual_Date] , [Format = "MMM-yy"] ), type text),
    col_ReturnDate = Table.AddColumn(col_PeriodReturn, "Return Date", each [Return] & " - " & Date.ToText( [Actual_Date] , [Format = "MMM-yy"] ), type text),
    col_isMonthComplete = Table.AddColumn(col_ReturnDate, "isMonthComplete", each Date.EndOfMonth([Actual_Date]) < Date.From(Date.EndOfMonth(var_CurrentDate)) , type logical),
    col_maxDate = Table.AddColumn(col_isMonthComplete, "Overlap Dates", each let
cond1 = [isMonthComplete],
cond2 = if cond1 and [FiscalYearOFFSET] = 0 then [ActualMonth_End] else null,
result = cond2
in
result, type date ),
  list_AllHeaders = Table.ColumnNames(col_FiscalQuarterYear),
  list_Columns = {
    "OG_MonthYear_ID",
    "ActualYearMonth_ID",
    "FiscalYearMonth_ID",
    "Fiscal Period",
    "Actual_Date",
    "Relative_Date",
    "ActualMonth_End",
    "RelativeMonth_End",
    "FiscalYear_NUM",
    "ActualYear_NUM",
    "FiscalMonth_NUM",
    "ActualMonth_NUM",
    "ActualMonth_Name",
    "RelativeMonth_NUM",
    "RelativeMonth_Name",
    "Return",
    "Month Period",
    "MonthOFFSET",
    "Quarter",
    "Fiscal Quarter",
    "FiscalQuarterOFFSET",
    "Fiscal Quarter & Year",
    "FiscalYearOFFSET"
  },
  cols_Reorder = Table.ReorderColumns(col_maxDate, list_Columns)
in
    cols_Reorder
, 
      // ------------------------------------------------------------------     
      /*
    minYear as number,
    maxYear as number, 
    MonthsInYear as number, 
    optional FYMonthNUM as number
*/
      // 3.0: change parameter metadata here
      fnType = type function (
        // 3.0.1: Calendar Start Year parameter
        minYear as (
          type number
            meta [
              Documentation.FieldCaption     = " Start Year: #(lf) (4-digit integer) ", 
              Documentation.FieldDescription = " 4-digit integer: #(cr,lf) YYYY ", 
              Documentation.SampleValues     = {2021}
            ]
        ), 
        // 3.0.2: Calendar End Year parameter
        maxYear as (
          type number
            meta [
              Documentation.FieldCaption     = " End Year: #(lf) (4-digit integer) ", 
              Documentation.FieldDescription = " 4-digit integer: #(cr,lf) YYYY ", 
              Documentation.SampleValues     = {2023}
            ]
        ), 
        // 3.0.3: Fiscal Start Month parameter
        MonthsInYear as (
          type number
            meta [
              Documentation.FieldCaption     = " Total Months in Period: #(lf) (eg: 14) ", 
              Documentation.FieldDescription = " Total Months in Custom Period: #(lf) (eg: 14) ", 
              Documentation.SampleValues     = {14}
            ]
        )// 3.0.4: Fiscal Start Month parameter
        , 
        optional FYMonthNUM as (
          type number
            meta [
              Documentation.FieldCaption     = " Fiscal Month Start: #(lf) (eg: Aug = 8) ", 
              Documentation.FieldDescription = " Fiscal Month Start: #(lf) (eg: Aug = 8, etc) ", 
              Documentation.SampleValues     = {123}
            ]
        )
      // 3.1: parameter return type   
      ) as list, 
      // ------------------------------------------------------------------
      // 4.0: edit function metadata here
      documentation = [
        // Inspired by SQLBI - Alberto Ferrari and Marco Russo (https://www.daxpatterns.com/custom-time-related-calculations/)
        Documentation.Name = " fnCustomPeriodCalendar ", 
        Documentation.Description = " Custom Calendar for non-standard Periods ", 
        Documentation.LongDescription = " Custom Calendar for non-standard Periods ", 
        Documentation.Category = " Calendar Category ", 
        Documentation.Source = "  PBIQUERYOUS  ", 
        Documentation.Version = " 1.0 (Custom Calendar for non-standard Periods (> 12 months))", 
        Documentation.Author = " Imran Haq ", 
        Documentation.Examples = {
          [
            Description = "  Custom Calendar for non-standard Periods   ", 
            Code = " fn_CustomPeriodCalendar(2021, 2024, 14, 8) ", 
            Result
              = "
 1. Input paramaters
 2. Invoke function
 3. Optional: replace start / end year parameters with dynamic values to automatically extend table
 
"
          ]
        /* ,
            [
            Description = "  description   ",
            Code    = " code ", 
            Result  = " result #(cr,lf) new line
                      #(cr,lf) new line #(cr,lf) 2 "
            ] */
        }
      ], 
      // ------------------------------------------------------------------
      // 5.0: Choose between Parameter Documentation or Function Documentation
      functionDocumentation =  // -- function metadata                       
      Value.ReplaceType(
        invokeFunction, 
        Value.ReplaceMetadata(Value.Type(invokeFunction), documentation)
      ), 
      parameterDocumentation =  // -- parameter metadata                        
      Value.ReplaceType(functionDocumentation, fnType)
    in
      // ------------------------------------------------------------------
      // select one of the above steps and paste below
      parameterDocumentation /* <-- Choose final documentation type */                                         
in
  customFunction