// For more information see https://aka.ms/fsharp-console-apps

// -r "nuget: FSharp.Data"
// -r "nuget: FSharp.Stats, 0.5.0"
open FSharp.Stats
open FSharp.Stats.Fitting

// -r "nuget: Deedle, 3.0.0"
// -r "nuget: Deedle.Interactive, 3.0.0"
open Deedle.Interactive
open Deedle

// -r "nuget: Plotly.NET, 4.2.0"
// -r "nuget: Plotly.NET.Interactive, 4.1.0"
open Plotly.NET.Interactive
open Plotly.NET
open Plotly.NET.LayoutObjects
open Plotly.NET.TraceObjects
open Plotly.NET.StyleParam

// -r "nuget: FSharpAux.IO, 2.0.0"
open FSharpAux.IO

// let currentDesktopPath = System.Environment.GetFolderPath(folder = System.Environment.SpecialFolder.Desktop)
// let currentDocumentPath = System.Environment.GetFolderPath(folder = System.Environment.SpecialFolder.MyDocuments) 
// let currentRepositryFolderPath = System.Environment.GetFolderPath(folder = System.Environment.SpecialFolder.MyDocuments) + @"/GitHub_Repository/AnalysisTool-for-bioreactor"
// let currentDirection = __SOURCE_DIRECTORY__
// let separator = System.IO.Path.PathSeparator

//Analysis full with horizontal split End Graph and only 3 row table
let analysis (fileName:string) (upperODCut:float) (lowerODCut:float) (cylinder : int list) =
    let currentProjectPath = 
        (__SOURCE_DIRECTORY__)
            //.Replace('C:\\' '')
            //.Replace('\\', '/')
            .TrimEnd([|'n';'o';'i';'t';'c';'n';'u';'F';'/';'\\';'c';'r';'s';'/';'\\';'s';'i';'s';'y';'l'|])
            .TrimEnd([|'a';'n';'A'|])
            .TrimEnd([|'/';'\\'|])         // "deletes /Analysis/src/Function from currentPath")
            //.Replace('/', '\\')
    let rawData : Frame<float,string> =  
        Frame.ReadCsv(location = (currentProjectPath + @"/InsertTableHere/" + fileName+".txt"),
        separators = "\t", 
        hasHeaders = true,
        inferRows = 10000 // damit die ersten 1000 zeilen genommen werden, um den Typ zu bestimmen
        // schema = 
        //     "actinic-lights.light-1-CW5700K = float, actinic-lights.light-2-CW5700K = float, actinic-lights.light-3-CW5700K = float, actinic-lights.light-4-CW5700K = float, actinic-lights.light-5-CW5700K = float, actinic-lights.light-6-CW5700K = float, actinic-lights.light-7-CW5700K = float, actinic-lights.light-8-CW5700K = float, od-1-sensors.od-680 = float, od-2-sensors.od-680 = float, od-3-sensors.od-680 = float, od-4-sensors.od-680 = float, od-5-sensors.od-680 = float, od-6-sensors.od-680 = float, od-7-sensors.od-680 = float, od-8-sensors.od-680 = float, pumps-20057.pump-1-consumption = float, pumps-20057.pump-2-consumption = float, pumps-20057.pump-3-consumption = float, pumps-20057.pump-4-consumption = float, pumps-20057.pump-5-consumption = float, pumps-20057.pump-6-consumption = float, pumps-20057.pump-7-consumption = float, pumps-20057.pump-8-consumption = float"
        )
        |> Frame.indexRows "time"

// Funktion pumpData and odData
    let getColumnsWithSubstring (frame: Frame<float,string>) (substring: string) =
        frame.ColumnKeys
        |> Seq.filter (fun x -> x.Contains(substring))

// Null Array
    let zeroArray = Array.init 250 (fun index -> (float index,0.0)) 

    let pump_Data = 
        let keys = getColumnsWithSubstring rawData ".pump"
        rawData
        |> Frame.sliceCols
            (keys |> Seq.sort)
        |> Frame.mapColKeys (fun x ->
                match x with
                | _ when x.Contains("-1-") -> "pump_data_1"
                | _ when x.Contains("-2-") -> "pump_data_2"
                | _ when x.Contains("-3-") -> "pump_data_3"
                | _ when x.Contains("-4-") -> "pump_data_4"
                | _ when x.Contains("-5-") -> "pump_data_5"
                | _ when x.Contains("-6-") -> "pump_data_6"
                | _ when x.Contains("-7-") -> "pump_data_7"
                | _ when x.Contains("-8-") -> "pump_data_8"
                | _ -> x + "_Not_related_with_AnyCylinder"
        )
            // [
            //     "pumps-20057.pump-1-consumption";
            //     "pumps-20057.pump-2-consumption";
            //     "pumps-20057.pump-3-consumption";
            //     "pumps-20057.pump-4-consumption";
            //     "pumps-20057.pump-5-consumption";
            //     "pumps-20057.pump-6-consumption";
            //     "pumps-20057.pump-7-consumption";
            //     "pumps-20057.pump-8-consumption"
            // ]
        |> Frame.fillMissing Direction.Forward

    let light_Data = 
        let keys = getColumnsWithSubstring rawData ".light" //|> Seq.sortWith (fun x -> x.Contains(num))
        rawData
        |> Frame.sliceCols
            (keys |> Seq.sort)
            |> Frame.mapColKeys (fun x -> 
                match x with
                | _ when x.Contains("-1-") -> "light_treatment_1"
                | _ when x.Contains("-2-") -> "light_treatment_2"
                | _ when x.Contains("-3-") -> "light_treatment_3"
                | _ when x.Contains("-4-") -> "light_treatment_4"
                | _ when x.Contains("-5-") -> "light_treatment_5"
                | _ when x.Contains("-6-") -> "light_treatment_6"
                | _ when x.Contains("-7-") -> "light_treatment_7"
                | _ when x.Contains("-8-") -> "light_treatment_8"
                | _ -> x
            )
            // [
            //     "actinic-lights.light-1-CW5700K";
            //     "actinic-lights.light-2-CW5700K";
            //     "actinic-lights.light-3-CW5700K";
            //     "actinic-lights.light-4-CW5700K";
            //     "actinic-lights.light-5-CW5700K";
            //     "actinic-lights.light-6-CW5700K";
            //     "actinic-lights.light-7-CW5700K";
            //     "actinic-lights.light-8-CW5700K"
            // ]
        |> Frame.fillMissing Direction.Forward

    let OD680_Data = 
        let keys = getColumnsWithSubstring rawData ".od" 
        rawData
        |> Frame.sliceCols
            (keys |> Seq.sort)
            |> Frame.mapColKeys (fun x -> 
                match x with
                | _ when x.Contains("-1-") -> "od_data_1"
                | _ when x.Contains("-2-") -> "od_data_2"
                | _ when x.Contains("-3-") -> "od_data_3"
                | _ when x.Contains("-4-") -> "od_data_4"
                | _ when x.Contains("-5-") -> "od_data_5"
                | _ when x.Contains("-6-") -> "od_data_6"
                | _ when x.Contains("-7-") -> "od_data_7"
                | _ when x.Contains("-8-") -> "od_data_8"
                | _ -> x
            )
            // [
            //     "od-sensors-1-.od-680";
            //     "od-sensors-2-.od-680";
            //     "od-sensors-3-.od-680";
            //     "od-sensors-4-.od-680";
            //     "od-sensors-5-.od-680";
            //     "od-sensors-6-.od-680";
            //     "od-sensors-7-.od-680";
            //     "od-sensors-8-.od-680"
            // ]
        |> Frame.mapValues (fun x -> log(x))

//Pump Data from cylinder 2,4,6,8 and all -> Series with tuple float (Time, Log OD680)
    let pump_Data_1  = //: (float,float) array  = 
        let pumpColumn =
            pump_Data.TryGetColumnObservation<float>("pump_data_1", Lookup.Exact)
        match pumpColumn.HasValue with
        | true -> pumpColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let pump_Data_2 = 
        let pumpColumn1 =
            pump_Data.TryGetColumnObservation<float>("pump_data_2",Lookup.Exact)
        match pumpColumn1.HasValue with
        | true -> pumpColumn1.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let pump_Data_3 = 
        let pumpColumn =
            pump_Data.TryGetColumnObservation<float>("pump_data_3",Lookup.Exact)
        match pumpColumn.HasValue with
        | true -> pumpColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let pump_Data_4 = 
        let pumpColumn =
            pump_Data.TryGetColumnObservation<float>("pump_data_4",Lookup.Exact)
        match pumpColumn.HasValue with
        | true -> pumpColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let pump_Data_5 = 
        let pumpColumn =
            pump_Data.TryGetColumnObservation<float>("pump_data_5",Lookup.Exact)
        match pumpColumn.HasValue with
        | true -> pumpColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let pump_Data_6 = 
        let pumpColumn =
            pump_Data.TryGetColumnObservation<float>("pump_data_6",Lookup.Exact)
        match pumpColumn.HasValue with
        | true -> pumpColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let pump_Data_7 = 
        let pumpColumn =
            pump_Data.TryGetColumnObservation<float>("pump_data_7",Lookup.Exact)
        match pumpColumn.HasValue with
        | true -> pumpColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let pump_Data_8 = 
        let pumpColumn =
            pump_Data.TryGetColumnObservation<float>("pump_data_8",Lookup.Exact)
        match pumpColumn.HasValue with
        | true -> pumpColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let pump_Data_all = 
        [|pump_Data_1;pump_Data_2;pump_Data_3;pump_Data_4;pump_Data_5;pump_Data_6;pump_Data_7;pump_Data_8|]
        |> Array.map (fun pumpArrayOfOneCylinder -> 
            pumpArrayOfOneCylinder
            |> Array.map (fun pumpvolume -> ((fst pumpvolume),((snd pumpvolume) - (snd (pumpArrayOfOneCylinder.[0])))))
        )

//OD680 Data from cylinder 2,4,6,8 and all -> Array with tuple float (Time, Log OD680)
    let OD680_Data_1 =
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_1",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let OD680_Data_2 = 
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_2",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let OD680_Data_3 = 
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_3",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let OD680_Data_4 = 
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_4",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let OD680_Data_5 = 
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_5",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let OD680_Data_6 = 
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_6",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let OD680_Data_7 = 
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_7",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let OD680_Data_8 = 
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_8",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let odData680_all = [|OD680_Data_1;OD680_Data_2;OD680_Data_3;OD680_Data_4;OD680_Data_5;OD680_Data_6;OD680_Data_7;OD680_Data_8|]

//light data 
    let light_Data_1 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_1",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let light_Data_2 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_2",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let light_Data_3 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_3",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let light_Data_4 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_4",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let light_Data_5 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_5",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let light_Data_6 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_6",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray
            

    let light_Data_7 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_7",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let light_Data_8 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_8",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> zeroArray

    let lightData_all = [|light_Data_1;light_Data_2;light_Data_3;light_Data_4;light_Data_5;light_Data_6;light_Data_7;light_Data_8|]

// lightphases -> array with tuple (starttime,endtime)
    let lightphase (lightData: (float*float) []) =
        let lightphaseTimeTupleStartEnd : (float * float) array= 
            lightData
            |> Array.groupBy (fun (timeLight,lightIntensity) -> lightIntensity)
            |> Array.filter (fun (constantIntensity,arrayTimeLight) -> arrayTimeLight.Length < 20000 && arrayTimeLight.Length > 300)
            |> Array.map (fun (constantVolume,arrayTimePumpvolume) ->
                (fst (arrayTimePumpvolume.[0]),fst (arrayTimePumpvolume |> Array.last)))
        if lightphaseTimeTupleStartEnd.Length = 3 then 
            [|(fst lightphaseTimeTupleStartEnd.[0], snd lightphaseTimeTupleStartEnd.[1]); (fst lightphaseTimeTupleStartEnd.[2], snd lightphaseTimeTupleStartEnd.[2])|]
        else
            lightphaseTimeTupleStartEnd

    let lightphase_1 =
        (lightphase light_Data_1)

    let lightphase_2 =
        (lightphase light_Data_2)

    let lightphase_3 =
        (lightphase light_Data_3)

    let lightphase_4 =
        (lightphase light_Data_4)

    let lightphase_5 =
        (lightphase light_Data_5)

    let lightphase_6 =
        (lightphase light_Data_6)

    let lightphase_7 =
        (lightphase light_Data_7)

    let lightphase_8 =
        (lightphase light_Data_8)

    let lightphase_all = [|lightphase_1;lightphase_2;lightphase_3;lightphase_4;lightphase_5;lightphase_6;lightphase_7;lightphase_8|]

    let sampleNumber odData =
            if odData = OD680_Data_1 then "1"
            else if odData = OD680_Data_2 then "2"
            else if odData = OD680_Data_3 then "3" 
            else if odData = OD680_Data_4 then "4" 
            else if odData = OD680_Data_5 then "5"
            else if odData = OD680_Data_6 then "6"
            else if odData = OD680_Data_7 then "7"  
            else if odData = OD680_Data_8 then "8"
            else "00"

// growphases -> srray with tuple (starttime,endtime)
    let growphase (pumpData: (float*float) array) (odData: (float*float) array) =
        let growphaseTimeTupleStartEnd = 
            let arrayOfConstantPumpValue =
                pumpData
                |> Array.groupBy (fun (timePump,pumpVolume) -> pumpVolume)
                |> Array.filter (fun (constantVolume,arrayTimePumpvolume) -> arrayTimePumpvolume.Length > 250)
                
            let startEndOfConstantPumpValue : (float * float) array =
                arrayOfConstantPumpValue
                |> Array.map (fun ((constantVolume:float),(arrayTimePumpvolume: (float * float) array)) -> 
                    if Array.isEmpty arrayTimePumpvolume then 
                        (0.0,0.0)
                    else
                        (float (fst (arrayTimePumpvolume.[0])),float (fst (arrayTimePumpvolume |> Array.last)))
                    )
            startEndOfConstantPumpValue
            |> Array.map (fun (startOfgrowphase,endOfGrowphase) -> 
                odData 
                |> Array.filter (fun (time,od680) -> time > startOfgrowphase && time < endOfGrowphase && od680 > (log lowerODCut) && od680 < (log upperODCut) )) //evtl bei Growphase einbauen damit shapes korrekt sind
                |> Array.map (fun (arrayTimeOD680) ->
                    if Array.isEmpty arrayTimeOD680 then 
                        let res = (0.0,0.0)
                        res
                    else
                    (fst (arrayTimeOD680.[0]), fst (arrayTimeOD680 |> Array.last)))
        growphaseTimeTupleStartEnd

    let growphase_1 =
        (growphase pump_Data_1 OD680_Data_1)

    let growphase_2 =
        (growphase pump_Data_2 OD680_Data_2)
    
    let growphase_3 =
        (growphase pump_Data_3 OD680_Data_3)
    
    let growphase_4 =
        (growphase pump_Data_4 OD680_Data_4)

    let growphase_5 =
        (growphase pump_Data_5 OD680_Data_5)

    let growphase_6 =
        (growphase pump_Data_6 OD680_Data_6)
    
    let growphase_7 =
        (growphase pump_Data_7 OD680_Data_7)
    
    let growphase_8 =
        (growphase pump_Data_8 OD680_Data_8)

    let growphase_all = [|growphase_1;growphase_2;growphase_3;growphase_4;growphase_5;growphase_6;growphase_7;growphase_8|]
 
// linear regression funktion 
    let ChartGrowphase_linearFit (growphase : (float * float) array) (odData:(float * float) array) (growphase_index : int) = 
        let arrayGrowphase_Time_OD (growphase : (float * float) array) (odData:(float * float) array) =
            growphase
            |> Array.map (fun (min,max) -> 
                odData 
                |> Array.filter (fun (time,od680) -> time > min && time < max && od680 > (log lowerODCut) && od680 < (log upperODCut) )) //evtl bei Growphase einbauen damit shapes korrekt sind

        let growphase_time_OD (growphase_index : int)=
            let growphase_time_OD = (arrayGrowphase_Time_OD growphase odData).[growphase_index]
            growphase_time_OD

        let xs (growphase_index : int) =
            let SeqOfOD680 =
                (growphase_time_OD growphase_index)
                |> Array.map (fun (time,od680) -> time)
            let vector_OD = vector SeqOfOD680
            vector_OD 

        let ys (growphase_index : int)=
            let SeqOfOD680 =
                (growphase_time_OD growphase_index)
                |> Array.map (fun (time,od680) -> od680)
            let vector_OD = vector SeqOfOD680
            vector_OD 
        let fit = 
            //if Vector.length (xs growphase_index) > 3 && Vector.length (ys growphase_index) > 3 then
                Fitting.LinearRegression.fit(xs growphase_index ,ys growphase_index,FittingMethod = Fitting.Method.Robust RobustEstimator.TheilSen)
            // else
            //     Fitting.LinearRegression.Coefficients(vector [0.0;0.0])
        let e : float = 
            // if Array.isEmpty (growphase_time_OD growphase_index) then
            //     0.0
            // else
                (growphase_time_OD growphase_index)
                |> Array.map (fun (time,od680) -> time)
                |> Array.last
        let s : float =
            // if Array.isEmpty (growphase_time_OD growphase_index) then
            //     0.0
            // else
                (growphase_time_OD growphase_index)
                |> Array.map (fun (time,od680) -> time)
                |> Array.sortDescending
                |> Array.last
        let linaer_fit = 
            [s .. 0.1 .. e] 
            |> List.map (fun x -> x,LinearRegression.predict(fit) x)  
            |> Chart.Line 
        linaer_fit


// shape list -> array of float tuple (starttime, Endtime)
    let shapeGrow (growphase : (float * float) array) (odData : (float * float) array) = 
        growphase
        |> Array.mapi (fun i (start,finish) -> 
            Shape.init (
                ShapeType = StyleParam.ShapeType.Rectangle,
                X0 = start,
                X1 = finish,
                Y0 = (snd (odData |> Array.sortBy snd |> Array.last) + 0.1),
                Y1 = (snd (odData |> Array.sortBy snd |> Array.last) + 0.3),
                Opacity = 0.2,
                FillColor = Color.fromHex "#a3e77f",
                Label = ShapeLabel.init(TextTemplate = $"{i+1}", TextAngle = TextAngle.Degrees 0.0, TextPosition = TextPosition.BottomCenter )
        )
        )
        |> Array.toList


    let shapeLight (lightphase : (float * float) array) (odData : (float * float) array) (lightdata : (float * float) array) =  
        // printfn "Length of lightphase: %d" lightdata.Length   
        // let treatment1and2Tuple =
        //     lightphase
        //     |> Array.map (fun (start, finish) -> 
        //         let groupedByTreatment =
        //             lightdata
        //             |> Array.filter (fun (time, lightData) -> time > start && time < finish)
        //             |> Array.groupBy (fun (time, lightData) -> lightData)
        //             |> Array.sortByDescending (fun (treatmentKey, lightArray) -> treatmentKey)
        //         let tuple =
        //             match Array.isEmpty groupedByTreatment with
        //             | true -> (0.0,0.0)  // or any default value you prefer
        //             | false -> (fst groupedByTreatment.[1], fst groupedByTreatment.[0])
        //         tuple
        //     )
        //     |> Array.item 0

        let treatment1and2Tuple =
            let groupedByTreatment = 
                lightdata
                |> Array.groupBy (fun (time, lightData) -> lightData)
                |> Array.sortByDescending (fun (lightTreatment,_) -> lightTreatment )
            (fst groupedByTreatment.[1],fst groupedByTreatment.[0])

        let maxYValue =
            match Array.isEmpty odData with
            | true -> 0.0  // or any default value you prefer
            | false -> (snd (odData |> Array.sortByDescending snd |> Array.last) + 0.25)

        lightphase
        |> Array.mapi (fun i (start,finish) -> 
                if i = 0 then 
                    Shape.init (
                        ShapeType = StyleParam.ShapeType.Rectangle,
                        X0 = start,
                        X1 = finish,
                        Y0 = maxYValue,
                        Y1 = 0.0, //(snd (odData |> Array.sortByDescending snd |> Array.last) + 1.0),
                        Opacity = 0.1,
                        FillColor = Color.fromHex "#ffffba",
                        Label = ShapeLabel.init(TextTemplate = $"Highlight {fst treatment1and2Tuple}", TextAngle = TextAngle.Degrees 0.0, TextPosition = TextPosition.BottomCenter )
                        )
                else
                    Shape.init (
                        ShapeType = StyleParam.ShapeType.Rectangle,
                        X0 = start,
                        X1 = finish,
                        Y0 = maxYValue,
                        Y1 = 0.0, //(snd (odData |> Array.sortByDescending snd |> Array.last) + 1.0),
                        Opacity = 0.1,
                        FillColor = Color.fromHex "#ffffba",
                        Label = ShapeLabel.init(TextTemplate = $"Highlight {snd treatment1and2Tuple}", TextAngle = TextAngle.Degrees 0.0, TextPosition = TextPosition.BottomCenter )
                        )
        )
        |> Array.toList

// pump-graph combined with OD graph and linear fit for growphase
    let endGraph (growphase : (float * float) array) (odData:(float * float) array) (pump_Data : (float * float) array) (lightData : (float * float) array)=
        let linearRegression_all_chartLine =
            [
                for i in 0 .. growphase.Length - 1 do
                (ChartGrowphase_linearFit growphase odData i) |> Chart.withTraceInfo $"Robust TheilSen {i}"
                |> Chart.withLineStyle(Width = 2.25, Color = Color.fromHex("#fb2e01"), Dash=StyleParam.DrawingStyle.Dot)
                |> Chart.withAxisAnchor(X=1,Y=1)
                |> Chart.withTraceInfo(Name = $"Phase ID: {i}")
            ]
            |> Chart.combine
        let odData_all_chartPoint =
            Chart.Point(xy = (odData))
            |> Chart.withLineStyle(Color =Color.fromHex("#7bc043"))
            |> Chart.withTraceInfo(Name = "Ln OD680")
            |> Chart.withAxisAnchor(X=1,Y=1)
        let pumpData_all_chartPoint = 
            Chart.Point(xy = (pump_Data))
            |> Chart.withLineStyle(Color =Color.fromHex("#005b96"))
            |> Chart.withTraceInfo(Name = "pumpvolume (ml)")
            |> Chart.withAxisAnchor(X=1,Y=2)
        let lightData_all_chartPoint = 
            Chart.Point(xy = (lightData))
            |> Chart.withLineStyle(Color =Color.fromHex("#ffffba"))
            |> Chart.withTraceInfo(Name = "Lighttreatment (light?)")
            |> Chart.withAxisAnchor(X=1,Y=2)
        [
            odData_all_chartPoint
            linearRegression_all_chartLine
            lightData_all_chartPoint
            pumpData_all_chartPoint
        ]
        |> Chart.combine
        |> Chart.withXAxisStyle("time (h)",
            Id=StyleParam.SubPlotId.XAxis 1,
            MinMax= (0.0, (fst (odData |> Array.last)) + 1.0),
            Side=StyleParam.Side.Bottom,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            $"Ln OD680 cylinder {sampleNumber odData}",
            Side=StyleParam.Side.Left,
            MinMax= (((snd (odData |> Array.sortByDescending snd |> Array.last)) + 0.25),((snd (odData |> Array.sortBy snd |> Array.last)) + 0.2)),
            Id=StyleParam.SubPlotId.YAxis 1,
            //Overlaying= StyleParam.LinearAxisId.Y 2,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            $"pumpvolume (ml) and light treatment (µE) cylinder {sampleNumber odData}", 
            Side=StyleParam.Side.Right,
            MinMax= (((snd (lightData |> Array.sortByDescending snd |> Array.last)) - 0.0),((snd (lightData |> Array.sortBy snd |> Array.last)) * 1.5)),
            Id=StyleParam.SubPlotId.YAxis 2,
            //Overlaying= StyleParam.LinearAxisId.Y 1,
            ShowLine=true,
            LineColor= Color.fromKeyword(Black)
            )

// pump-graph combined with OD graph and linear fit for growphase
    let endGraph_single (growphase : (float * float) array) (lightphase : (float * float) array) (odData:(float * float) array) (pump_Data : (float * float) array) (lightData : (float * float) array) =
        let linearRegression_all_chartLine =
            [
                for i in 0 .. growphase.Length - 1 do
                (ChartGrowphase_linearFit growphase odData i) |> Chart.withTraceInfo $"Robust TheilSen {i}"
                |> Chart.withLineStyle(Width = 2.25, Color = Color.fromHex("#fb2e01"), Dash=StyleParam.DrawingStyle.Dot)
                |> Chart.withAxisAnchor(Y=1)
                |> Chart.withTraceInfo(Name = $"Phase ID: {i}")
            ]
            |> Chart.combine
        let odData_all_chartPoint =
            Chart.Point(xy = (odData))
            |> Chart.withLineStyle(Color =Color.fromHex("#7bc043"))
            |> Chart.withTraceInfo(Name = "Ln OD680")
            |> Chart.withAxisAnchor(Y=1)
        let pumpData_all_chartPoint = 
            Chart.Point(xy = (pump_Data))
            |> Chart.withLineStyle(Color =Color.fromHex("#005b96"))
            |> Chart.withTraceInfo(Name = "pumpvolume (ml)")
            |> Chart.withAxisAnchor(Y=2)
        let lightData_all_chartPoint = 
            Chart.Point(xy = (lightData))
            |> Chart.withLineStyle(Color =Color.fromHex("#ffffba"))
            |> Chart.withTraceInfo(Name = "Lighttreatment (µE)")
            |> Chart.withAxisAnchor(Y=2)
        [
            odData_all_chartPoint
            linearRegression_all_chartLine
            lightData_all_chartPoint
            pumpData_all_chartPoint
        ]
        |> Chart.combine
        |> Chart.withShapes(shapes = (shapeGrow growphase odData), Append = true)
        |> Chart.withShapes(shapes = (shapeLight lightphase odData lightData), Append = true)
        |> Chart.withTemplate ChartTemplates.lightMirrored
        |> Chart.withLegendStyle(Orientation = StyleParam.Orientation.Horizontal)
        |> Chart.withSize(Width = 1600, Height = 800)
        |> Chart.withTitle($"OD680 with linear regression of the growphases of cylinder {sampleNumber odData} with pumpdata")
        |> Chart.withXAxisStyle("time (h)",
            Id=StyleParam.SubPlotId.XAxis 1,
            MinMax= (0.0, (fst (odData |> Array.last)) + 1.0),
            Side=StyleParam.Side.Bottom,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            $"Ln OD680 cylinder {sampleNumber odData}",
            Side=StyleParam.Side.Left,
            MinMax= (((snd (odData |> Array.sortByDescending snd |> Array.last)) + 0.25),((snd (odData |> Array.sortBy snd |> Array.last)) + 0.2)),
            Id=StyleParam.SubPlotId.YAxis 1,
            Overlaying= StyleParam.LinearAxisId.Y 2,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            $"pumpvolume (ml) and light treatment (µE) cylinder {sampleNumber odData}", 
            Side=StyleParam.Side.Right,
            MinMax= (((snd (lightData |> Array.sortByDescending snd |> Array.last)) - 0.0),((snd (lightData |> Array.sortBy snd |> Array.last)) * 1.5)),
            Id=StyleParam.SubPlotId.YAxis 2,
            //Overlaying= StyleParam.LinearAxisId.Y 1,
            ShowLine=false,
            LineColor= Color.fromKeyword(Black)
            )


// slope/growratefunction for individual growphase of one cylinder
    let slopeOfGrowphase_X (growphase : (float * float) array) (odData:(float * float) array) (growphase_index : int) = 
        let arrayGrowphase_Time_OD (growphase : (float * float) array) (odData:(float * float) array) =
            growphase
            |> Array.map (fun (min,max) -> 
                odData 
                |> Array.filter (fun (time,od680) -> time > min && time < max && od680 > (log lowerODCut) && od680 < (log upperODCut)))

        let growphase_time_OD (growphase_index : int)=
            let growphase_time_OD = (arrayGrowphase_Time_OD growphase odData).[growphase_index]
            growphase_time_OD

        let xs (growphase_index : int) =
            let SeqOfOD680 =
                (growphase_time_OD growphase_index)
                |> Array.map (fun (time,od680) -> time)
            let vector_OD = vector SeqOfOD680
            vector_OD 

        let ys (growphase_index : int)=
            let SeqOfOD680 =
                (growphase_time_OD growphase_index)
                |> Array.map (fun (time,od680) -> od680)
            let vector_OD = vector SeqOfOD680
            vector_OD 
        let fit = 
            //if Vector.length (xs growphase_index) <> 0 && Vector.length (ys growphase_index) <> 0 then
                Fitting.LinearRegression.fit(xs growphase_index ,ys growphase_index,FittingMethod = Fitting.Method.Robust RobustEstimator.TheilSen)
            // else
            //     Fitting.LinearRegression.Coefficients(vector [0.0;0.0])
        fit.Coefficients.[1]

// function for table values -> List List string
    let table_list (growphase : (float * float) array) (odData:(float * float) array) : list<Library.tableRow> = 
        let growrateList =
            [
                for i in 0 .. growphase.Length - 1 do
                (slopeOfGrowphase_X growphase odData i)
            ]
            |> List.toArray
        let list_int = 
            [|1 .. growphase.Length|]
        let start = 
            growphase
            |> Array.map (fun (start,finish) -> start)
        let finish = 
            growphase
            |> Array.map (fun (start,finish) -> finish)
        let rowItemsList : Library.tableRow list =
            [for i in 0 .. growphase.Length - 1 do
                let list_int_index: int = list_int.[i]
                let start_index: float = start.[i]
                let finish_index: float = finish.[i]
                let growrate_index: float = growrateList.[i]
                let duplicationTime: float = (log(2.0) /(growrate_index))
                {
                    Library.tableRow.PhaseID = list_int_index;
                    Library.tableRow.startTimeGrowphase = start_index;
                    Library.tableRow.endTimeGrowphase = finish_index;
                    Library.tableRow.slopeOrGrowrateOfLinearRegressionOrGrowphase = growrate_index;
                    Library.tableRow.duplicationTimeOfGrowphase = duplicationTime
                }
            ]
        //let headerItems = []
        rowItemsList

// function for boxblot of slopes of one cylinder
    let boxplot_slope (growphase : (float * float) array) (odData:(float * float) array) = 
        let y = 
            table_list (growphase) (odData) |> List.map (fun x -> float x.slopeOrGrowrateOfLinearRegressionOrGrowphase)
        Chart.BoxPlot(Y = y, Name = "growrate (h<sup>-1</sub>)",Jitter=0.1,BoxPoints=StyleParam.BoxPoints.SuspectedOutliers,BoxMean=StyleParam.BoxMean.True, OutlineWidth = 1.5)
        |> Chart.withLineStyle(Color = Color.fromHex("#005b96"))

// function for pointchart of slopes of one cylinder
    let pointchart_slope (growphase : (float * float) array) (odData:(float * float) array) = 
        let xy = 
            table_list (growphase) (odData) |> Seq.map (fun x -> (float x.PhaseID, float x.slopeOrGrowrateOfLinearRegressionOrGrowphase)) 
        Chart.Point(xy = xy, Name = "growrate (h<sup>-1</sub>)")
        |> Chart.withLineStyle(Color = Color.fromHex("#005b96"))

    let sortSlopeList (growphase : (float * float) array) (odData:(float * float) array) = 
        let slopelist = 
            (table_list growphase odData)
            |> List.map (fun rowItem -> (rowItem.slopeOrGrowrateOfLinearRegressionOrGrowphase))
        slopelist
        |> List.sort

// function for table
    let table (growphase : (float * float) array) (odData:(float * float) array) = 
        let header : seq<string> = seq [ "<b>Phase ID</b>"; "starttime of growphase (h)"; "endtime of growphase (h)"; "growrate (h<sup>-1</sub>)"; "duplicationtime (Td) (h)" ]
        let rows : seq<seq<float>> = 
            table_list growphase odData
            |> Seq.map (fun rowItem ->
                    seq [
                        float rowItem.PhaseID;
                        round 2 rowItem.startTimeGrowphase;
                        round 2 rowItem.endTimeGrowphase;
                        round 3 rowItem.slopeOrGrowrateOfLinearRegressionOrGrowphase;
                        round 3 rowItem.duplicationTimeOfGrowphase
                        ]
                )
        Chart.Table(
            header,
            rows,
            HeaderAlign = StyleParam.HorizontalAlign.Left,
            CellsAlign = StyleParam.HorizontalAlign.Left,
            MultiColumnWidth = [1.5;2.5;2.5;4.0],
            HeaderFillColor = Color.fromString( "DarkGray" ),
            HeaderHeight = 2,
            HeaderOutlineColor = Color.fromString( "Grey" ),
            HeaderOutlineWidth = 2.0,
            CellsFillColor = Color.fromString( "LightGray" ),
            CellsOutlineColor = Color.fromString( "Grey" ),
            CellsOutlineWidth = 1.5,
            CellsHeight = 25
            )
        |> Chart.withSize(Width=1600,Height=800)

// function for table
    let description (growphase : (float * float) array) (odData:(float * float) array) = 
        // some styling for a html table
        let style = "<style>table {font-family: arial, sans-serif;border-collapse: collapse;width: 80%;height: 100%}td, th {border: 2px solid #dddddd;text-align: left;padding: 8px;}tr:nth-child(even) {background-color: #dddddd;}</style>"
        // header row of the table
        let header = "<tr><th>Phase ID</th><th>starttime of growphase (h)</th><th>endtime of growphase (h)</th><th>growrate (h<sup>-1</sup>)</th><th>duplicationtime (h)</th></tr>"
        // table rows
        let rows = 
            (table_list growphase odData)
            |> List.map (fun rowItem ->
                    // create a table row with phase id, the start and end of the treatment formatted as a float with one significant figure (defined by %.1f) and the slope with four significant figures.
                    // the slope is stored within the fit coefficients as [intersect;slope]
                $"<tr><td>{rowItem.PhaseID}</td><td>%.2f{(rowItem.startTimeGrowphase)}</td><td>%.2f{(rowItem.endTimeGrowphase)}</td><td>%.4f{(rowItem.slopeOrGrowrateOfLinearRegressionOrGrowphase)}</td><td>%.4f{(rowItem.duplicationTimeOfGrowphase)}</td></tr>"
            )
        // constructed table
        let table = $"{style}<table>{header}{rows}</table>"
        // convert the table string to a giraffe node element to be compatible with Plotly.NET
        Giraffe.ViewEngine.HtmlElements.rawText table

// analysis function 
    let analysisFull (growphase : (float * float) array) (lightphase : (float * float) array) (odData:(float * float) array) (pump_Data :(float * float) array) (lightData : (float * float) array) =

        let subplotGrid = 
            [|
                [| StyleParam.LinearAxisId.X 1 , StyleParam.LinearAxisId.Y 1 ; StyleParam.LinearAxisId.X 2 , StyleParam.LinearAxisId.Y 2 |]
                [| StyleParam.LinearAxisId.X 3 , StyleParam.LinearAxisId.Y 3 ; StyleParam.LinearAxisId.X 4 , StyleParam.LinearAxisId.Y 4 |]
            |]

        let pumpData_all_chartPoint = 
            Chart.Point(xy = (pump_Data))
            |> Chart.withLineStyle(Color =Color.fromHex("#005b96"))
            |> Chart.withTraceInfo(Name = "pumpvolume (ml)")

        [
            
            (endGraph (growphase) (odData) (pump_Data) (lightData))
            |> Chart.withTemplate ChartTemplates.lightMirrored
            |> Chart.withShapes(shapes = (shapeGrow growphase odData), Append = true)
            |> Chart.withAxisAnchor(X=1,Y=1);

            //(endGraph (growphase) (odData) (pump_Data))
            pumpData_all_chartPoint
            |> Chart.withTemplate ChartTemplates.lightMirrored
            //|> Chart.withLayoutStyle(PlotBGColor = Color.fromARGB 0 0 0 0, PaperBGColor = Color.fromARGB 0 0 0 0)
            |> Chart.withAxisAnchor(X=1,Y=2);
        
            (pointchart_slope (growphase) (odData))
            |> Chart.withTemplate ChartTemplates.lightMirrored
            |> Chart.withAxisAnchor(X=3,Y=3);

            (boxplot_slope (growphase) (odData))
            |> Chart.withTemplate ChartTemplates.lightMirrored
            |> Chart.withAxisAnchor(X=4,Y=4);

            table growphase odData
            |> Chart.withTemplate ChartTemplates.lightMirrored
            //|> Chart.withAxisAnchor(X=5,Y=5);

        ]
        |> Chart.Grid(nRows=3, nCols=2, Pattern = StyleParam.LayoutGridPattern.Independent)
        |> Chart.withTemplate ChartTemplates.lightMirrored
        |> Chart.withShapes(shapes = (shapeGrow growphase odData), Append = true)
        |> Chart.withShapes(shapes = (shapeLight lightphase odData lightData), Append = true)
        |> Chart.withLegendStyle(Orientation = StyleParam.Orientation.Horizontal)
        |> Chart.withXAxisStyle("",
            Id=StyleParam.SubPlotId.XAxis 1,
            MinMax= (0.0, (fst (odData |> Array.last)) + 1.0),
            Domain = (0.00, 1.00),
            Side=StyleParam.Side.Bottom,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            $"Ln OD680 cylinder {sampleNumber odData}",
            Side=StyleParam.Side.TopLeft,
            MinMax= (((snd (odData |> Array.sortByDescending snd |> Array.last)) + 0.25 ),((snd (odData |> Array.sortBy snd |> Array.last)) + 0.2)),
            Id=StyleParam.SubPlotId.YAxis 1,
            Domain = (0.725, 1.00),
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            $"pumpvolume (ml) cylinder {sampleNumber odData}", 
            Side=StyleParam.Side.Right,
            MinMax= (((snd (pump_Data |> Array.sortByDescending snd |> Array.last)) - 0.05),((snd (pump_Data |> Array.sortBy snd |> Array.last)) * 1.)),
            Id=StyleParam.SubPlotId.YAxis 2,
            //Overlaying=StyleParam.LinearAxisId.Y 1,
            Domain = (0.625, 0.725),
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withXAxisStyle("time (h)",
            Id=StyleParam.SubPlotId.XAxis 2,
            MinMax= (0.0, (fst (odData |> Array.last)) + 1.0),
            Domain = (0.00, 1.00),
            Side=StyleParam.Side.Bottom,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withXAxisStyle($"growphases in cylinder {sampleNumber odData}",
            Id=StyleParam.SubPlotId.XAxis 3,
            MinMax= (0.0, growphase.Length + 1),
            Domain = (0.00, 0.49),
            Side=StyleParam.Side.Bottom,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle("growrate (h<sup>-1</sub>)",
            Id=StyleParam.SubPlotId.YAxis 3,
            MinMax= (((sortSlopeList growphase odData).[0]) - 0.005, ((sortSlopeList growphase odData) |> List.last) + 0.005),
            Domain = (0.325, 0.55),
            Side=StyleParam.Side.Left,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withXAxisStyle($"cylinder {sampleNumber odData}",
            Id=StyleParam.SubPlotId.XAxis 4,
            Domain = (0.51, 1.00),
            Side=StyleParam.Side.Bottom,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )      
        |> Chart.withYAxisStyle("growrate (h<sup>-1</sub>)",
            Id=StyleParam.SubPlotId.YAxis 4,
            MinMax= (((sortSlopeList growphase odData).[0]) - 0.005, ((sortSlopeList growphase odData) |> List.last) + 0.005),
            Domain = (0.325, 0.55),
            Side=StyleParam.Side.Right,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withXAxisStyle("",
            Id=StyleParam.SubPlotId.XAxis 5,
            Domain = (0.00, 0.33),
            Side=StyleParam.Side.Bottom,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            "",
            Side=StyleParam.Side.TopLeft,
            Id=StyleParam.SubPlotId.YAxis 5,
            Domain = (0.00, 1.00),
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withLayoutGridStyle (YGap = 0.3, XGap = 0.1, SubPlots = subplotGrid)
        |> Chart.withLayoutStyle(ShowLegend = false)
        |> Chart.withLayoutStyle(Font=(Font.init(Family= StyleParam.FontFamily.Arial,Size=14)))
        |> Chart.withSize (1500, 1600)
        |> Chart.withConfig (Config.init (ToImageButtonOptions = ConfigObjects.ToImageButtonOptions.init(Format = StyleParam.ImageFormat.SVG)))
        //|> Chart.withDescription [description growphase odData]
        |> Chart.withTitle($"growphases analysis of cylinder {sampleNumber odData}")
    let tableExport (growphase : (float * float) array) (odData:(float * float) array) =
        FileIO.writeToFile true (currentProjectPath + @"/Output/" + $"tableOfCylinder_{sampleNumber odData}" + ".csv") ( (table_list growphase odData) |> Seq.map (fun rowItem -> $"{rowItem.PhaseID};%.2f{(rowItem.startTimeGrowphase)};%.2f{(rowItem.endTimeGrowphase)};%.4f{(rowItem.slopeOrGrowrateOfLinearRegressionOrGrowphase)};%.4f{(rowItem.duplicationTimeOfGrowphase)}"))
    let tableshow (growphase : (float * float) array) (odData:(float * float) array) =
        table growphase odData
        |> Chart.withTitle($"table: growphase characteristics of cylinder {sampleNumber odData}")
//do analysisFull for all 8 cylinder
    for i in cylinder do
        (tableExport growphase_all.[i] odData680_all.[i])
        (endGraph_single growphase_all.[i] lightphase_all.[i] odData680_all.[i] pump_Data_all.[i] lightData_all.[i])
        |> Chart.saveHtml(path = (currentProjectPath + @"/Output/" + $"simpleAnalysisOfCylinder_{(sampleNumber odData680_all.[i])}" + ".html"), OpenInBrowser = false )
        //(description growphase_all.[i] odData680_all.[i]) |> Giraffe.ViewEngine.RenderView.AsString.xmlNode;
        (analysisFull growphase_all.[i] lightphase_all.[i] odData680_all.[i] pump_Data_all.[i] lightData_all.[i]) 
        |> Chart.saveHtml(path = (currentProjectPath + @"/Output/" + $"advancedAnalysisOfCylinder_{(sampleNumber odData680_all.[i])}" + ".html"), OpenInBrowser = false )
        (analysisFull growphase_all.[i] lightphase_all.[i] odData680_all.[i] pump_Data_all.[i] lightData_all.[i]) 
        |> Chart.show
        (endGraph_single growphase_all.[i] lightphase_all.[i] odData680_all.[i] pump_Data_all.[i] lightData_all.[i])
        |> Chart.show


// let analysisTest (fileName:string) (upperODCut:float) (lowerODCut:float) (cylinder : int list) : Result<unit, string> =
//     // match (analysis fileName upperODCut lowerODCut cylinder) with
//     // | _ -> failwith "ohoh"
//     // | () -> Ok()
//     Ok(analysis fileName upperODCut lowerODCut cylinder)
    
    
        
let analysisTest (fileName:string) (upperODCut:float) (lowerODCut:float) (cylinder : int list) : Result<unit, string> =
    let currentProjectPath = 
        (__SOURCE_DIRECTORY__)
            .TrimEnd([|'n';'o';'i';'t';'c';'n';'u';'F';'/';'c';'r';'s';'/';'s';'i';'s';'y';'l'|])
            .TrimEnd([|'a';'n';'A';'/'|])         // "deletes /Analysis/src/Function from currentPath")
    let rawData : Frame<float,string> =  
        Frame.ReadCsv(location = (currentProjectPath + @"/InsertTableHere/" + fileName + ".txt"),
        separators = "\t",
        hasHeaders = true,
        inferRows = 10000 // damit die ersten 1000 zeilen genommen werden, um den Typ zu bestimmen
        // schema = 
        //     "actinic-lights.light-1-CW5700K = float, actinic-lights.light-2-CW5700K = float, actinic-lights.light-3-CW5700K = float, actinic-lights.light-4-CW5700K = float, actinic-lights.light-5-CW5700K = float, actinic-lights.light-6-CW5700K = float, actinic-lights.light-7-CW5700K = float, actinic-lights.light-8-CW5700K = float, od-1-sensors.od-680 = float, od-2-sensors.od-680 = float, od-3-sensors.od-680 = float, od-4-sensors.od-680 = float, od-5-sensors.od-680 = float, od-6-sensors.od-680 = float, od-7-sensors.od-680 = float, od-8-sensors.od-680 = float, pumps-20057.pump-1-consumption = float, pumps-20057.pump-2-consumption = float, pumps-20057.pump-3-consumption = float, pumps-20057.pump-4-consumption = float, pumps-20057.pump-5-consumption = float, pumps-20057.pump-6-consumption = float, pumps-20057.pump-7-consumption = float, pumps-20057.pump-8-consumption = float"
        )
        |> Frame.indexRows "time"

// Funktion pumpData and odData
    let getColumnsWithSubstring (frame: Frame<float,string>) (substring: string) =
        frame.ColumnKeys
        |> Seq.filter (fun x -> x.Contains(substring))

    let pump_Data = 
        let keys = getColumnsWithSubstring rawData ".pump"
        rawData
        |> Frame.sliceCols
            (keys |> Seq.sort)
        |> Frame.mapColKeys (fun x ->
                match x with
                | _ when x.Contains("-1-") -> "pump_data_1"
                | _ when x.Contains("-2-") -> "pump_data_2"
                | _ when x.Contains("-3-") -> "pump_data_3"
                | _ when x.Contains("-4-") -> "pump_data_4"
                | _ when x.Contains("-5-") -> "pump_data_5"
                | _ when x.Contains("-6-") -> "pump_data_6"
                | _ when x.Contains("-7-") -> "pump_data_7"
                | _ when x.Contains("-8-") -> "pump_data_8"
                | _ -> x + "_Not_related_with_AayCylinder"
        )
            // [
            //     "pumps-20057.pump-1-consumption";
            //     "pumps-20057.pump-2-consumption";
            //     "pumps-20057.pump-3-consumption";
            //     "pumps-20057.pump-4-consumption";
            //     "pumps-20057.pump-5-consumption";
            //     "pumps-20057.pump-6-consumption";
            //     "pumps-20057.pump-7-consumption";
            //     "pumps-20057.pump-8-consumption"
            // ]
        |> Frame.fillMissing Direction.Forward

    let light_Data = 
        let keys = getColumnsWithSubstring rawData ".light" //|> Seq.sortWith (fun x -> x.Contains(num))
        rawData
        |> Frame.sliceCols
            (keys |> Seq.sort)
            |> Frame.mapColKeys (fun x -> 
                match x with
                | _ when x.Contains("-1-") -> "light_treatment_1"
                | _ when x.Contains("-2-") -> "light_treatment_2"
                | _ when x.Contains("-3-") -> "light_treatment_3"
                | _ when x.Contains("-4-") -> "light_treatment_4"
                | _ when x.Contains("-5-") -> "light_treatment_5"
                | _ when x.Contains("-6-") -> "light_treatment_6"
                | _ when x.Contains("-7-") -> "light_treatment_7"
                | _ when x.Contains("-8-") -> "light_treatment_8"
                | _ -> x
            )
            // [
            //     "actinic-lights.light-1-CW5700K";
            //     "actinic-lights.light-2-CW5700K";
            //     "actinic-lights.light-3-CW5700K";
            //     "actinic-lights.light-4-CW5700K";
            //     "actinic-lights.light-5-CW5700K";
            //     "actinic-lights.light-6-CW5700K";
            //     "actinic-lights.light-7-CW5700K";
            //     "actinic-lights.light-8-CW5700K"
            // ]
        |> Frame.fillMissing Direction.Forward

    let OD680_Data = 
        let keys = getColumnsWithSubstring rawData ".od" 
        rawData
        |> Frame.sliceCols
            (keys |> Seq.sort)
            |> Frame.mapColKeys (fun x -> 
                match x with
                | _ when x.Contains("-1-") -> "od_data_1"
                | _ when x.Contains("-2-") -> "od_data_2"
                | _ when x.Contains("-3-") -> "od_data_3"
                | _ when x.Contains("-4-") -> "od_data_4"
                | _ when x.Contains("-5-") -> "od_data_5"
                | _ when x.Contains("-6-") -> "od_data_6"
                | _ when x.Contains("-7-") -> "od_data_7"
                | _ when x.Contains("-8-") -> "od_data_8"
                | _ -> x
            )
            // [
            //     "od-sensors-1.od-680";
            //     "od-sensors-2.od-680";
            //     "od-sensors-3.od-680";
            //     "od-sensors-4.od-680";
            //     "od-sensors-5.od-680";
            //     "od-sensors-6.od-680";
            //     "od-sensors-7.od-680";
            //     "od-sensors-8.od-680"
            // ]
        //|> Frame.mapValues (fun x -> log(x))
        //|> Frame.getNumericCols

//Pump Data from cylinder 2,4,6,8 and all -> Series with tuple float (Time, Log OD680)
    let pump_Data_1  = //: (float,float) array  = 
        let pumpColumn =
            pump_Data.TryGetColumnObservation<float>("pump_data_1", Lookup.Exact)
        match pumpColumn.HasValue with
        | true -> pumpColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|] 

    let pump_Data_2 = 
        let pumpColumn1 =
            pump_Data.TryGetColumnObservation<float>("pump_data_2",Lookup.Exact)
        match pumpColumn1.HasValue with
        | true -> pumpColumn1.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|] 

    let pump_Data_3 = 
        let pumpColumn =
            pump_Data.TryGetColumnObservation<float>("pump_data_3",Lookup.Exact)
        match pumpColumn.HasValue with
        | true -> pumpColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|] 

    let pump_Data_4 = 
        let pumpColumn =
            pump_Data.TryGetColumnObservation<float>("pump_data_4",Lookup.Exact)
        match pumpColumn.HasValue with
        | true -> pumpColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|] 

    let pump_Data_5 = 
        let pumpColumn =
            pump_Data.TryGetColumnObservation<float>("pump_data_5",Lookup.Exact)
        match pumpColumn.HasValue with
        | true -> pumpColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|] 

    let pump_Data_6 = 
        let pumpColumn =
            pump_Data.TryGetColumnObservation<float>("pump_data_6",Lookup.Exact)
        match pumpColumn.HasValue with
        | true -> pumpColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|] 

    let pump_Data_7 = 
        let pumpColumn =
            pump_Data.TryGetColumnObservation<float>("pump_data_7",Lookup.Exact)
        match pumpColumn.HasValue with
        | true -> pumpColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|] 

    let pump_Data_8 = 
        let pumpColumn =
            pump_Data.TryGetColumnObservation<float>("pump_data_8",Lookup.Exact)
        match pumpColumn.HasValue with
        | true -> pumpColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|] 

    let pump_Data_all = [|pump_Data_1;pump_Data_2;pump_Data_3;pump_Data_4;pump_Data_5;pump_Data_6;pump_Data_7;pump_Data_8|]

//OD680 Data from cylinder 2,4,6,8 and all -> Array with tuple float (Time, Log OD680)
    let OD680_Data_1 =
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_1",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let OD680_Data_2 = 
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_2",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let OD680_Data_3 = 
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_3",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let OD680_Data_4 = 
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_4",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let OD680_Data_5 = 
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_5",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let OD680_Data_6 = 
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_6",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let OD680_Data_7 = 
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_7",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let OD680_Data_8 = 
        let OD_Column =
            OD680_Data.TryGetColumnObservation<float>("od_data_8",Lookup.Exact)
        match OD_Column.HasValue with
        | true -> OD_Column.Value.Value |> Series.dropMissing |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let odData680_all = [|OD680_Data_1;OD680_Data_2;OD680_Data_3;OD680_Data_4;OD680_Data_5;OD680_Data_6;OD680_Data_7;OD680_Data_8|]

//light data 
    let light_Data_1 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_1",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let light_Data_2 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_2",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let light_Data_3 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_3",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let light_Data_4 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_4",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let light_Data_5 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_5",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let light_Data_6 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_6",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let light_Data_7 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_7",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let light_Data_8 = 
        let lightColumn =
            light_Data.TryGetColumnObservation<float>("light_treatment_8",Lookup.Exact)
        match lightColumn.HasValue with
        | true -> lightColumn.Value.Value |> Series.observations |> Seq.toArray
        | false -> [|0.0,0.0|]  

    let lightData_all = [|light_Data_1;light_Data_2;light_Data_3;light_Data_4;light_Data_5;light_Data_6;light_Data_7;light_Data_8|]

// lightphases -> srray with tuple (starttime,endtime)
    let lightphase (lightData: (float*float) []) =
        let lightphaseTimeTupleStartEnd = 
            lightData
            |> Array.groupBy (fun (timeLight,lightIntensity) -> lightIntensity)
            |> Array.filter (fun (constantIntensity,arrayTimeLight) -> arrayTimeLight.Length < 20000 && arrayTimeLight.Length > 300)
            |> Array.map (fun (constantVolume,arrayTimePumpvolume) ->
                (fst (arrayTimePumpvolume.[0]),fst (arrayTimePumpvolume |> Array.last)))
        lightphaseTimeTupleStartEnd

    let lightphase_1 =
        (lightphase light_Data_1)

    let lightphase_2 =
        (lightphase light_Data_2)

    let lightphase_3 =
        (lightphase light_Data_3)

    let lightphase_4 =
        (lightphase light_Data_4)

    let lightphase_5 =
        (lightphase light_Data_5)

    let lightphase_6 =
        (lightphase light_Data_6)

    let lightphase_7 =
        (lightphase light_Data_7)

    let lightphase_8 =
        (lightphase light_Data_8)

    let lightphase_all = [|lightphase_1;lightphase_2;lightphase_3;lightphase_4;lightphase_5;lightphase_6;lightphase_7;lightphase_8|]

    let sampleNumber odData =
            if odData = OD680_Data_1 then "1"
            else if odData = OD680_Data_2 then "2"
            else if odData = OD680_Data_3 then "3" 
            else if odData = OD680_Data_4 then "4" 
            else if odData = OD680_Data_5 then "5"
            else if odData = OD680_Data_6 then "6"
            else if odData = OD680_Data_7 then "7"  
            else if odData = OD680_Data_8 then "8"
            else "00"

// growphases -> srray with tuple (starttime,endtime)
    let growphase (pumpData: (float*float) array) (odData: (float*float) array) =
        let growphaseTimeTupleStartEnd = 
            let arrayOfConstantPumpValue =
                pumpData
                |> Array.groupBy (fun (timePump,pumpVolume) -> pumpVolume)
                |> Array.filter (fun (constantVolume,arrayTimePumpvolume) -> arrayTimePumpvolume.Length > 250)
                
            let startEndOfConstantPumpValue : (float * float) array =
                arrayOfConstantPumpValue
                |> Array.map (fun ((constantVolume:float),(arrayTimePumpvolume: (float * float) array)) -> 
                    if Array.isEmpty arrayTimePumpvolume then 
                        let res = (0.0,0.0)
                        res
                    else
                        (float (fst (arrayTimePumpvolume.[0])),float (fst (arrayTimePumpvolume |> Array.last)))
                    )
            startEndOfConstantPumpValue
            |> Array.map (fun (startOfgrowphase,endOfGrowphase) -> 
                odData 
                |> Array.filter (fun (time,od680) -> time > startOfgrowphase && time < endOfGrowphase && od680 > (log lowerODCut) && od680 < (log upperODCut) )) //evtl bei Growphase einbauen damit shapes korrekt sind
                |> Array.map (fun (arrayTimeOD680) ->
                    if Array.isEmpty arrayTimeOD680 then 
                        let res = (0.0,0.0)
                        res
                    else
                    (fst (arrayTimeOD680.[0]), fst (arrayTimeOD680 |> Array.last)))
        growphaseTimeTupleStartEnd

    let growphase_1 =
        (growphase pump_Data_1 OD680_Data_1)

    let growphase_2 =
        (growphase pump_Data_2 OD680_Data_2)
    
    let growphase_3 =
        (growphase pump_Data_3 OD680_Data_3)
    
    let growphase_4 =
        (growphase pump_Data_4 OD680_Data_4)

    let growphase_5 =
        (growphase pump_Data_5 OD680_Data_5)

    let growphase_6 =
        (growphase pump_Data_6 OD680_Data_6)
    
    let growphase_7 =
        (growphase pump_Data_7 OD680_Data_7)
    
    let growphase_8 =
        (growphase pump_Data_8 OD680_Data_8)

    let growphase_all = [|growphase_1;growphase_2;growphase_3;growphase_4;growphase_5;growphase_6;growphase_7;growphase_8|]
 
// linear regression funktion 
    let ChartGrowphase_linearFit (growphase : (float * float) array) (odData:(float * float) array) (growphase_index : int) = 
        let arrayGrowphase_Time_OD (growphase : (float * float) array) (odData:(float * float) array) =
            growphase
            |> Array.map (fun (min,max) -> 
                odData 
                |> Array.filter (fun (time,od680) -> time > min && time < max && od680 > (log lowerODCut) && od680 < (log upperODCut) )) //evtl bei Growphase einbauen damit shapes korrekt sind

        let growphase_time_OD (growphase_index : int)=
            let growphase_time_OD = (arrayGrowphase_Time_OD growphase odData).[growphase_index]
            growphase_time_OD

        let xs (growphase_index : int) =
            let SeqOfOD680 =
                (growphase_time_OD growphase_index)
                |> Array.map (fun (time,od680) -> time)
            let vector_OD = vector SeqOfOD680
            vector_OD 

        let ys (growphase_index : int)=
            let SeqOfOD680 =
                (growphase_time_OD growphase_index)
                |> Array.map (fun (time,od680) -> od680)
            let vector_OD = vector SeqOfOD680
            vector_OD 
        let fit = 
            //if Vector.length (xs growphase_index) > 3 && Vector.length (ys growphase_index) > 3 then
                Fitting.LinearRegression.fit(xs growphase_index ,ys growphase_index,FittingMethod = Fitting.Method.Robust RobustEstimator.TheilSen)
            // else
            //     Fitting.LinearRegression.Coefficients(vector [0.0;0.0])
        let e : float = 
            // if Array.isEmpty (growphase_time_OD growphase_index) then
            //     0.0
            // else
                (growphase_time_OD growphase_index)
                |> Array.map (fun (time,od680) -> time)
                |> Array.last
        let s : float =
            // if Array.isEmpty (growphase_time_OD growphase_index) then
            //     0.0
            // else
                (growphase_time_OD growphase_index)
                |> Array.map (fun (time,od680) -> time)
                |> Array.sortDescending
                |> Array.last
        let linaer_fit = 
            [s .. 0.1 .. e] 
            |> List.map (fun x -> x,LinearRegression.predict(fit) x)  
            |> Chart.Line 
        linaer_fit


// shape list -> array of float tuple (starttime, Endtime)
    let shapeGrow (growphase : (float * float) array) (odData : (float * float) array) = 
        growphase
        |> Array.mapi (fun i (start,finish) -> 
            Shape.init (
                ShapeType = StyleParam.ShapeType.Rectangle,
                X0 = start,
                X1 = finish,
                Y0 = (snd (odData |> Array.sortBy snd |> Array.last) + 0.1),
                Y1 = (snd (odData |> Array.sortBy snd |> Array.last) + 0.3),
                Opacity = 0.2,
                FillColor = Color.fromHex "#a3e77f",
                Label = ShapeLabel.init(TextTemplate = $"{i+1}", TextAngle = TextAngle.Degrees 0.0, TextPosition = TextPosition.BottomCenter )
        )
        )
        |> Array.toList

    let shapeLight (lightphase : (float * float) array) (odData : (float * float) array) = 
        lightphase
        |> Array.mapi (fun i (start,finish) -> 
            Shape.init (
                ShapeType = StyleParam.ShapeType.Rectangle,
                X0 = start,
                X1 = finish,
                Y0 = (snd (odData |> Array.sortByDescending snd |> Array.last) - 0.3),
                Y1 = (snd (odData |> Array.sortByDescending snd |> Array.last) + 1.5),
                Opacity = 0.1,
                FillColor = Color.fromHex "#ffffba",
                Label = ShapeLabel.init(TextTemplate = $"Light", TextAngle = TextAngle.Degrees 0.0, TextPosition = TextPosition.MiddleCenter )
        )
        )
        |> Array.toList

// pump-graph combined with OD graph and linear fit for growphase
    let endGraph (growphase : (float * float) array) (odData:(float * float) array) (pump_Data : (float * float) array) (lightData : (float * float) array)=
        let linearRegression_all_chartLine =
            [
                for i in 0 .. growphase.Length - 1 do
                (ChartGrowphase_linearFit growphase odData i) |> Chart.withTraceInfo $"Robust TheilSen {i}"
                |> Chart.withLineStyle(Width = 10.5, Color = Color.fromHex("#fb2e01"), Dash=StyleParam.DrawingStyle.Dot)
                |> Chart.withAxisAnchor(X=1,Y=1)
                |> Chart.withTraceInfo(Name = $"Phase ID: {i}")
            ]
            |> Chart.combine
        let odData_all_chartPoint =
            Chart.Point(xy = (odData))
            |> Chart.withLineStyle(Color =Color.fromHex("#7bc043"))
            |> Chart.withTraceInfo(Name = "Ln OD680")
            |> Chart.withAxisAnchor(X=1,Y=1)
        let pumpData_all_chartPoint = 
            Chart.Point(xy = (pump_Data))
            |> Chart.withLineStyle(Color =Color.fromHex("#005b96"))
            |> Chart.withTraceInfo(Name = "pumpvolume (ml)")
            |> Chart.withAxisAnchor(X=1,Y=2)
        let lightData_all_chartPoint = 
            Chart.Point(xy = (lightData))
            |> Chart.withLineStyle(Color =Color.fromHex("#ffffba"))
            |> Chart.withTraceInfo(Name = "Lighttreatment (light?)")
            |> Chart.withAxisAnchor(X=1,Y=3)
        [
            lightData_all_chartPoint;
            pumpData_all_chartPoint;
            odData_all_chartPoint;
            linearRegression_all_chartLine;
        ]
        |> Chart.combine
        |> Chart.withXAxisStyle("time (h)",
            Id=StyleParam.SubPlotId.XAxis 1,
            MinMax= (0.0, (fst (odData |> Array.last)) + 1.0),
            Side=StyleParam.Side.Bottom,
            ShowLine=false,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            $"Ln OD680 cylinder {sampleNumber odData}",
            Side=StyleParam.Side.TopLeft,
            MinMax= (((snd (odData |> Array.sortByDescending snd |> Array.last)) + 0.25),((snd (odData |> Array.sortBy snd |> Array.last)) + 0.2)),
            Id=StyleParam.SubPlotId.YAxis 1,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            $"pumpvolume (ml) cylinder {sampleNumber odData}", 
            Side=StyleParam.Side.Left,
            MinMax= (((snd (pump_Data |> Array.sortByDescending snd |> Array.last)) - 0.05),((snd (pump_Data |> Array.sortBy snd |> Array.last)) * 1.5)),
            Id=StyleParam.SubPlotId.YAxis 2,
            Overlaying=StyleParam.LinearAxisId.Y 1,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            $"light treatment (light?) cylinder {sampleNumber odData}", 
            Side=StyleParam.Side.Left,
            MinMax= (((snd (lightData |> Array.sortByDescending snd |> Array.last)) - 0.05),((snd (lightData |> Array.sortBy snd |> Array.last)) * 1.5)),
            Id=StyleParam.SubPlotId.YAxis 3,
            Overlaying=StyleParam.LinearAxisId.Y 2,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )

// pump-graph combined with OD graph and linear fit for growphase
    let endGraph_single (growphase : (float * float) array) (lightphase : (float * float) array) (odData:(float * float) array) (pump_Data : (float * float) array) (lightData : (float * float) array) =
        let linearRegression_all_chartLine =
            [
                for i in 0 .. growphase.Length - 1 do
                (ChartGrowphase_linearFit growphase odData i) |> Chart.withTraceInfo $"Robust TheilSen {i}"
                |> Chart.withLineStyle(Width = 10.5, Color = Color.fromHex("#fb2e01"), Dash=StyleParam.DrawingStyle.Dot)
                |> Chart.withAxisAnchor(Y=1)
                |> Chart.withTraceInfo(Name = $"Phase ID: {i}")
            ]
            |> Chart.combine
        let odData_all_chartPoint =
            Chart.Point(xy = (odData))
            |> Chart.withLineStyle(Color =Color.fromHex("#7bc043"))
            |> Chart.withTraceInfo(Name = "Ln OD680")
            |> Chart.withAxisAnchor(Y=1)
        let pumpData_all_chartPoint = 
            Chart.Point(xy = (pump_Data))
            |> Chart.withLineStyle(Color =Color.fromHex("#005b96"))
            |> Chart.withTraceInfo(Name = "pumpvolume (ml)")
            |> Chart.withAxisAnchor(Y=2)
        let lightData_all_chartPoint = 
            Chart.Point(xy = (lightData))
            |> Chart.withLineStyle(Color =Color.fromHex("#ffffba"))
            |> Chart.withTraceInfo(Name = "Lighttreatment (light?)")
            |> Chart.withAxisAnchor(X=1,Y=3)
        [
            lightData_all_chartPoint
            pumpData_all_chartPoint
            odData_all_chartPoint
            linearRegression_all_chartLine
        ]
        |> Chart.combine
        |> Chart.withShapes(shapes = (shapeGrow growphase odData), Append = true)
        |> Chart.withShapes(shapes = (shapeLight lightphase odData), Append = true)
        |> Chart.withTemplate ChartTemplates.lightMirrored
        |> Chart.withLegendStyle(Orientation = StyleParam.Orientation.Horizontal)
        |> Chart.withSize(Width = 1600, Height = 800)
        |> Chart.withTitle($"OD680 with linear regression of the growphases of cylinder {sampleNumber odData} with pumpdata")
        |> Chart.withYAxisStyle(
            $"Ln OD680 cylinder {sampleNumber odData}",
            Side=StyleParam.Side.Left,
            MinMax= (((snd (odData |> Array.sortByDescending snd |> Array.last)) + 0.2),((snd (odData |> Array.sortBy snd |> Array.last)) + 0.2)),
            Id=StyleParam.SubPlotId.YAxis 1,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withXAxisStyle("time (h)",
            Id=StyleParam.SubPlotId.XAxis 1,
            MinMax= (0.0, (fst (odData |> Array.last)) + 1.0),
            Side=StyleParam.Side.Bottom,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            $"pumpvolume (ml) cylinder {sampleNumber odData}", 
            Side=StyleParam.Side.Right,
            MinMax= (((snd (pump_Data |> Array.sortByDescending snd |> Array.last)) - 0.05),((snd (pump_Data |> Array.sortBy snd |> Array.last)) * 1.5)),
            Id=StyleParam.SubPlotId.YAxis 2,
            Overlaying=StyleParam.LinearAxisId.Y 1,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            $"light treatment (light?) cylinder {sampleNumber odData}", 
            Side=StyleParam.Side.Left,
            MinMax= (((snd (lightData |> Array.sortByDescending snd |> Array.last)) - 0.05),((snd (lightData |> Array.sortBy snd |> Array.last)) * 1.5)),
            Id=StyleParam.SubPlotId.YAxis 3,
            Overlaying=StyleParam.LinearAxisId.Y 2,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )

// slopefunction for individual growphase of one cylinder
    let slopeOfGrowphase_X (growphase : (float * float) array) (odData:(float * float) array) (growphase_index : int) = 
        let arrayGrowphase_Time_OD (growphase : (float * float) array) (odData:(float * float) array) =
            growphase
            |> Array.map (fun (min,max) -> 
                odData 
                |> Array.filter (fun (time,od680) -> time > min && time < max && od680 > (log lowerODCut) && od680 < (log upperODCut)))

        let growphase_time_OD (growphase_index : int)=
            let growphase_time_OD = (arrayGrowphase_Time_OD growphase odData).[growphase_index]
            growphase_time_OD

        let xs (growphase_index : int) =
            let SeqOfOD680 =
                growphase_time_OD growphase_index
                |> Array.map (fun (time,od680) -> time)
            let vector_OD = vector SeqOfOD680
            vector_OD 

        let ys (growphase_index : int)=
            let SeqOfOD680 =
                growphase_time_OD growphase_index
                |> Array.map (fun (time,od680) -> od680)
            let vector_OD = vector SeqOfOD680
            vector_OD 
        let fit = 
            if Vector.exists (fun x -> x = 2) (xs growphase_index) && Vector.exists (fun x -> x = 2) (ys growphase_index) then
                Fitting.LinearRegression.fit(xs growphase_index ,ys growphase_index,FittingMethod = Fitting.Method.Robust RobustEstimator.TheilSen)
            else
                Fitting.LinearRegression.Coefficients(vector [0.0;0.0])
        fit.Coefficients.[1]

// function for table values -> List List string
    let table_list (growphase : (float * float) array) (odData:(float * float) array) : list<Library.tableRow> = 
        let growrateList =
            [
                for i in 0 .. growphase.Length - 1 do
                (slopeOfGrowphase_X growphase odData i)
            ]
            |> List.toArray
        let list_int = 
            [|1 .. growphase.Length|]
        let start = 
            growphase
            |> Array.map (fun (start,finish) -> start)
        let finish = 
            growphase
            |> Array.map (fun (start,finish) -> finish)
        let rowItemsList : Library.tableRow list =
            [for i in 0 .. growphase.Length - 1 do
                let list_int_index: int = list_int.[i]
                let start_index: float = start.[i]
                let finish_index: float = finish.[i]
                let growrate_index: float = growrateList.[i]
                let duplicationTime: float = (log(2.0) /(growrate_index))
                {
                    Library.tableRow.PhaseID = list_int_index;
                    Library.tableRow.startTimeGrowphase = start_index;
                    Library.tableRow.endTimeGrowphase = finish_index;
                    Library.tableRow.slopeOrGrowrateOfLinearRegressionOrGrowphase = growrate_index;
                    Library.tableRow.duplicationTimeOfGrowphase = duplicationTime
                }
            ]
        //let headerItems = []
        rowItemsList

// function for boxblot of slopes of one cylinder
    let boxplot_slope (growphase : (float * float) array) (odData:(float * float) array) = 
        let y = 
            table_list (growphase) (odData) |> List.map (fun x -> float x.slopeOrGrowrateOfLinearRegressionOrGrowphase)
        Chart.BoxPlot(Y = y, Name = "growrate (h<sup>-1</sub>)",Jitter=0.1,BoxPoints=StyleParam.BoxPoints.SuspectedOutliers,BoxMean=StyleParam.BoxMean.True, OutlineWidth = 1.5)
        |> Chart.withLineStyle(Color = Color.fromHex("#005b96"))

// function for pointchart of slopes of one cylinder
    let pointchart_slope (growphase : (float * float) array) (odData:(float * float) array) = 
        let xy = 
            table_list (growphase) (odData) |> Seq.map (fun x -> (float x.PhaseID, float x.slopeOrGrowrateOfLinearRegressionOrGrowphase)) 
        Chart.Point(xy = xy, Name = "growrate (h<sup>-1</sub>)")
        |> Chart.withLineStyle(Color = Color.fromHex("#005b96"))

    let sortSlopeList (growphase : (float * float) array) (odData:(float * float) array) = 
        let slopelist = 
            (table_list growphase odData)
            |> List.map (fun rowItem -> (rowItem.slopeOrGrowrateOfLinearRegressionOrGrowphase))
        slopelist
        |> List.sort

// function for table
    let table (growphase : (float * float) array) (odData:(float * float) array) = 
        let header : seq<string> = seq [ "<b>Phase ID</b>"; "starttime of growphase (h)"; "endtime of growphase (h)"; "growrate (h<sup>-1</sub>)"; "duplicationtime (h)" ]
        let rows : seq<seq<float>> = 
            table_list growphase odData
            |> Seq.map (fun rowItem ->
                    seq [
                        float rowItem.PhaseID;
                        round 2 rowItem.startTimeGrowphase;
                        round 2 rowItem.endTimeGrowphase;
                        round 3 rowItem.slopeOrGrowrateOfLinearRegressionOrGrowphase;
                        round 3 rowItem.duplicationTimeOfGrowphase
                        ]
                )
        Chart.Table(
            header,
            rows,
            HeaderAlign = StyleParam.HorizontalAlign.Left,
            CellsAlign = StyleParam.HorizontalAlign.Left,
            MultiColumnWidth = [1.5;2.5;2.5;4.0],
            HeaderFillColor = Color.fromString( "DarkGray" ),
            HeaderHeight = 2,
            HeaderOutlineColor = Color.fromString( "Grey" ),
            HeaderOutlineWidth = 2.0,
            CellsFillColor = Color.fromString( "LightGray" ),
            CellsOutlineColor = Color.fromString( "Grey" ),
            CellsOutlineWidth = 1.5,
            CellsHeight = 25
            )
        |> Chart.withSize(Width=1600,Height=800)

// function for table
    let description (growphase : (float * float) array) (odData:(float * float) array) = 
        // some styling for a html table
        let style = "<style>table {font-family: arial, sans-serif;border-collapse: collapse;width: 80%;height: 100%}td, th {border: 2px solid #dddddd;text-align: left;padding: 8px;}tr:nth-child(even) {background-color: #dddddd;}</style>"
        
        // header row of the table
        let header = "<tr><th>Phase ID</th><th>starttime of growphase (h)</th><th>endtime of growphase (h)</th><th>growrate (h<sup>-1</sup>)</th><th>duplicationtime (h)</th></tr>"
        
        // table rows
        let rows = 
            (table_list growphase odData)
            |> List.map (fun rowItem ->
                    // create a table row with phase id, the start and end of the treatment formatted as a float with one significant figure (defined by %.1f) and the slope with four significant figures.
                    // the slope is stored within the fit coefficients as [intersect;slope]
                $"<tr><td>{rowItem.PhaseID}</td><td>%.2f{(rowItem.startTimeGrowphase)}</td><td>%.2f{(rowItem.endTimeGrowphase)}</td><td>%.4f{(rowItem.slopeOrGrowrateOfLinearRegressionOrGrowphase)}</td><td>%.4f{(rowItem.duplicationTimeOfGrowphase)}</td></tr>"
            )
        // constructed table
        let table = $"{style}<table>{header}{rows}</table>"

        // convert the table string to a giraffe node element to be compatible with Plotly.NET
        Giraffe.ViewEngine.HtmlElements.rawText table







// analysis function 
    let analysisFull (growphase : (float * float) array) (lightphase : (float * float) array) (odData:(float * float) array) (pump_Data :(float * float) array) (lightData : (float * float) array) =

        let subplotGrid = 
            [|
                [| StyleParam.LinearAxisId.X 1 , StyleParam.LinearAxisId.Y 1 ; StyleParam.LinearAxisId.X 2 , StyleParam.LinearAxisId.Y 2 |]
                [| StyleParam.LinearAxisId.X 3 , StyleParam.LinearAxisId.Y 3 ; StyleParam.LinearAxisId.X 4 , StyleParam.LinearAxisId.Y 4 |]
            |]

        let pumpData_all_chartPoint = 
            Chart.Point(xy = (pump_Data))
            |> Chart.withLineStyle(Color =Color.fromHex("#005b96"))
            |> Chart.withTraceInfo(Name = "pumpvolume (ml)")

        [
            
            (endGraph (growphase) (odData) (pump_Data) (lightData))
            |> Chart.withTemplate ChartTemplates.lightMirrored
            |> Chart.withShapes(shapes = (shapeGrow growphase odData), Append = true)
            |> Chart.withAxisAnchor(X=1,Y=1);

            //(endGraph (growphase) (odData) (pump_Data))
            pumpData_all_chartPoint
            |> Chart.withTemplate ChartTemplates.lightMirrored
            //|> Chart.withLayoutStyle(PlotBGColor = Color.fromARGB 0 0 0 0, PaperBGColor = Color.fromARGB 0 0 0 0)
            |> Chart.withAxisAnchor(X=1,Y=2);
        
            (pointchart_slope (growphase) (odData))
            |> Chart.withTemplate ChartTemplates.lightMirrored
            |> Chart.withAxisAnchor(X=3,Y=3);

            (boxplot_slope (growphase) (odData))
            |> Chart.withTemplate ChartTemplates.lightMirrored
            |> Chart.withAxisAnchor(X=4,Y=4);

            table growphase odData
            |> Chart.withTemplate ChartTemplates.lightMirrored
            //|> Chart.withAxisAnchor(X=5,Y=5);

        ]
        |> Chart.Grid(nRows=3, nCols=2, Pattern = StyleParam.LayoutGridPattern.Independent)
        |> Chart.withTemplate ChartTemplates.lightMirrored
        |> Chart.withShapes(shapes = (shapeGrow growphase odData), Append = true)
        |> Chart.withShapes(shapes = (shapeLight lightphase odData), Append = true)
        |> Chart.withLegendStyle(Orientation = StyleParam.Orientation.Horizontal)
        |> Chart.withXAxisStyle("",
            Id=StyleParam.SubPlotId.XAxis 1,
            MinMax= (0.0, (fst (odData |> Array.last)) + 1.0),
            Domain = (0.00, 1.00),
            Side=StyleParam.Side.Bottom,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            $"Ln OD680 cylinder {sampleNumber odData}",
            Side=StyleParam.Side.TopLeft,
            MinMax= (((snd (odData |> Array.sortByDescending snd |> Array.last)) + 0.25 ),((snd (odData |> Array.sortBy snd |> Array.last)) + 0.2)),
            Id=StyleParam.SubPlotId.YAxis 1,
            Domain = (0.725, 1.00),
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            $"pumpvolume (ml) cylinder {sampleNumber odData}", 
            Side=StyleParam.Side.Right,
            MinMax= (((snd (pump_Data |> Array.sortByDescending snd |> Array.last)) - 0.05),((snd (pump_Data |> Array.sortBy snd |> Array.last)) * 1.)),
            Id=StyleParam.SubPlotId.YAxis 2,
            //Overlaying=StyleParam.LinearAxisId.Y 1,
            Domain = (0.625, 0.725),
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withXAxisStyle("time (h)",
            Id=StyleParam.SubPlotId.XAxis 2,
            MinMax= (0.0, (fst (odData |> Array.last)) + 1.0),
            Domain = (0.00, 1.00),
            Side=StyleParam.Side.Bottom,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withXAxisStyle($"growphases in cylinder {sampleNumber odData}",
            Id=StyleParam.SubPlotId.XAxis 3,
            MinMax= (0.0, growphase.Length + 1),
            Domain = (0.00, 0.49),
            Side=StyleParam.Side.Bottom,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle("growrate (h<sup>-1</sub>)",
            Id=StyleParam.SubPlotId.YAxis 3,
            MinMax= (((sortSlopeList growphase odData).[0]) - 0.005, ((sortSlopeList growphase odData) |> List.last) + 0.005),
            Domain = (0.325, 0.55),
            Side=StyleParam.Side.Left,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withXAxisStyle($"cylinder {sampleNumber odData}",
            Id=StyleParam.SubPlotId.XAxis 4,
            Domain = (0.51, 1.00),
            Side=StyleParam.Side.Bottom,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )      
        |> Chart.withYAxisStyle("growrate (h<sup>-1</sub>)",
            Id=StyleParam.SubPlotId.YAxis 4,
            MinMax= (((sortSlopeList growphase odData).[0]) - 0.005, ((sortSlopeList growphase odData) |> List.last) + 0.005),
            Domain = (0.325, 0.55),
            Side=StyleParam.Side.Right,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withXAxisStyle("",
            Id=StyleParam.SubPlotId.XAxis 5,
            Domain = (0.00, 0.33),
            Side=StyleParam.Side.Bottom,
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withYAxisStyle(
            "",
            Side=StyleParam.Side.TopLeft,
            Id=StyleParam.SubPlotId.YAxis 5,
            Domain = (0.00, 1.00),
            ShowLine=true,
            LineColor=Color.fromKeyword Black
            )
        |> Chart.withLayoutGridStyle (YGap = 0.3, XGap = 0.1, SubPlots = subplotGrid)
        |> Chart.withLayoutStyle(ShowLegend = false)
        |> Chart.withLayoutStyle(Font=(Font.init(Family= StyleParam.FontFamily.Arial,Size=14)))
        |> Chart.withSize (1500, 1600)
        |> Chart.withConfig (Config.init (ToImageButtonOptions = ConfigObjects.ToImageButtonOptions.init(Format = StyleParam.ImageFormat.SVG)))
        //|> Chart.withDescription [description growphase odData]
        |> Chart.withTitle($"growphases analysis of cylinder {sampleNumber odData}")
    let tableExport (growphase : (float * float) array) (odData:(float * float) array) =
        FileIO.writeToFile true (currentProjectPath + @"/Output/" + $"tableOfCylinder_{sampleNumber odData}" + ".csv") ( (table_list growphase odData) |> Seq.map (fun rowItem -> $"{rowItem.PhaseID};%.2f{(rowItem.startTimeGrowphase)};%.2f{(rowItem.endTimeGrowphase)};%.4f{(rowItem.slopeOrGrowrateOfLinearRegressionOrGrowphase)};%.4f{(rowItem.duplicationTimeOfGrowphase)}"))
    let tableshow (growphase : (float * float) array) (odData:(float * float) array) =
        table growphase odData
        |> Chart.withTitle($"table: growphase characteristics of cylinder {sampleNumber odData}")
//do analysisFull for all 8 cylinder
    Ok ()