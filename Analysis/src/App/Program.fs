// For more information see https://aka.ms/fsharp-console-apps
open System
open System.Threading
open Spectre.Console


[<EntryPoint>]
let rec main argv =
    // let currentProjectPathByApp = 
    //     (__SOURCE_DIRECTORY__)
    //         .TrimEnd([|'p';'p';'A';'/';'c';'s';'r';'/';'s';'i';'s';'y';'l';'a'|])
    //         .TrimEnd([|'n';'A';'/'|])           // "deletes /Analysis/src/App from currentPath")
    //Console.BackgroundColor <- ConsoleColor.Black
    Console.ForegroundColor <- ConsoleColor.DarkGray
    //AnsiConsole.Profile()
    AnsiConsole.Clear()
    Console.SetWindowSize(90 ,50)
    AnsiConsole.Write(new FigletText(Color = Color.LightSteelBlue1,text = "AnalysisTool"))
    AnsiConsole.Markup 
        "\n Please [underline]enter the filename[/] of the data table\n [gray](';' separated textfile, filename without '.txt')[/]\n [gray](needs all 8 columns for pump-, light-, and OD-data),[/]\n also give an [underline]upper and lower OD threshold[/] for the calculation of the growphase,\n additionaly [underline]add the cylinder you want to analyse[/] [gray](choose 1-8 or all)\n (press SPACE between filename and the values)[/]\n\n [lightsteelblue3][bold]Example:[/] 202310_TR16_HLexperiment_RawData_Venny_Simon 0.44 0.36 3[/]\n\n [gray]Write [underline]exit[/] as input to quit the application [/]\n\n"
    AnsiConsole.Write(new Rule("[blue]Analysis[/]"))
    AnsiConsole.MarkupLine"\n[bold]Input:[/]"

    let input = 
        let lineInput = Console.ReadLine()
        lineInput.Split([|' '|])

    try
        match input with
        | [|"exit"|] when input.Length = 1 ->
            AnsiConsole.MarkupLine "\n\n Thanks for using my AnalysisTool, i hope you liked it! \n [blue bold]If you want to start again type [underline]dotnet run[/] and press enter.[/] \n\n"
        | _ when input.Length <> 4 -> 
            AnsiConsole.MarkupLine "\n\n [red bold]Invalid input.[/] Look into the introduction or see Example above.\n\n"

            let restart = 
                AnsiConsole.Confirm(prompt = "\n\n[yellow]Do you want to try again?[/]\n\n")

            if restart = true then main argv |> ignore else AnsiConsole.MarkupLine "\n\n Thanks for using my AnalysisTool, i hope you liked it! \n [blue bold]If you want to start again type [underline]dotnet run[/] and press enter.[/] \n\n"

        | [| fileName; upperThreshold; lowerThreshold; cylinder |] when input.Length = 4 ->

            AnsiConsole.Status()
                //.Spinner(Spinner.Known.SimpleDotsScrolling)
                //.SpinnerStyle(Style.Parse("green bold"))
                .Start("\n\n[bold][slowblink]Loading...[/][/]\n", fun ctx ->
                    Thread.Sleep(2000);
                )

            AnsiConsole.Status()
                //.Spinner(Spinner.Known.SimpleDotsScrolling)
                // .SpinnerStyle(Style.Parse("green bold"))
                .Start("Read in input", fun ctx ->
                    AnsiConsole.MarkupLine("\n\nLOG: Read data ...");
                    Thread.Sleep(1000);
                    //Update the status and spinner
                    //ctx.Status("Next task");
                    // ctx.Spinner(Spinner.Known.Star);
                    // ctx.SpinnerStyle(Style.Parse("green"));
                )

            let isFailed result =
                match result with
                | Ok _ -> false
                | Error _ -> true
            
            let resultForFailTest = AnalysisFunction.analysisTest fileName (float upperThreshold) (float lowerThreshold) (if cylinder = "all" then [0..7] else [int cylinder - 1 .. int cylinder - 1])

            AnsiConsole.Status()
                // .Spinner(Spinner.Known.SimpleDotsScrolling)
                // .SpinnerStyle(Style.Parse("green bold"))
                .Start("Read in data input", fun ctx ->
                    AnsiConsole.MarkupLine("LOG: Setup OD thresholds ...");
                    Thread.Sleep(1000);
                    //Update the status and spinner
                    //ctx.Status("Next task");
                    // ctx.Spinner(Spinner.Known.Star);
                    // ctx.SpinnerStyle(Style.Parse("green"));

                    AnsiConsole.MarkupLine("LOG: Choose cylinder(s) ...");
                    Thread.Sleep(1000);
                    //Update the status and spinner
                    //ctx.Status("Next task");
                    // ctx.Spinner(Spinner.Known.Star);
                    // ctx.SpinnerStyle(Style.Parse("green"));
                )

            if isFailed resultForFailTest then
                Console.WriteLine(Failure "")
            else
                //AnsiConsole.MarkupLine "\n"
                AnsiConsole.Status()
                    .Start("Read in data input", fun ctx ->
                        AnsiConsole.MarkupLine("LOG: Loading light treatment data ...");
                        Thread.Sleep(1000);
                        //Update the status and spinner
                        //ctx.Status("Next task");
                        // ctx.Spinner(Spinner.Known.Star);
                        // ctx.SpinnerStyle(Style.Parse("green"));

                        AnsiConsole.MarkupLine("LOG: Loading OD messurement data ...");
                        Thread.Sleep(1000);
                        //Update the status and spinner
                        // ctx.Status("Next task");
                        // ctx.Spinner(Spinner.Known.Star);
                        // ctx.SpinnerStyle(Style.Parse("green"));

                        AnsiConsole.MarkupLine("LOG: Loading medium pump volume data ...");
                        Thread.Sleep(1000);
                        //Update the status and spinner
                        // ctx.Status("Next task");
                        // ctx.Spinner(Spinner.Known.Star);
                        // ctx.SpinnerStyle(Style.Parse("green"));
                    );
                AnsiConsole.MarkupLine "\n [bold green]Valid Input![/]\n"     
                Thread.Sleep(1000)
                AnsiConsole.MarkupLine $"[bold lightsteelblue3]File:[/] {fileName}"
                Thread.Sleep(500)
                AnsiConsole.MarkupLine $"[bold lightsteelblue3]Upper OD Threshold:[/] {upperThreshold}"
                Thread.Sleep(500)
                AnsiConsole.MarkupLine $"[bold lightsteelblue3]Lower OD Threshold:[/] {lowerThreshold}"        
                Thread.Sleep(500)
                AnsiConsole.MarkupLine $"[bold lightsteelblue3]Cylinder:[/] {cylinder}"

                let progress =
                    AnsiConsole.Progress()
                        .Start(fun ctx ->
                            // Define tasks
                            let task1 = ctx.AddTask("[green3]Calculating Log Data[/]")
                            let task2 = ctx.AddTask("[green3]Analyse linear Regression[/]")                                    
                            let task3 = ctx.AddTask("[green3]Plotting graphes[/]")
                            let task4 = ctx.AddTask("[green3]Saving analysis data table[/]")

                            while not ctx.IsFinished do 
                                task1.Increment(2.0)
                                Thread.Sleep(25)
                                task2.Increment(1.75)
                                Thread.Sleep(25)
                                task3.Increment(1.5)
                                Thread.Sleep(25)
                                task4.Increment(1.25)
                            )
                progress

                let analysis = AnalysisFunction.analysis fileName (float upperThreshold) (float lowerThreshold) (if cylinder = "all" then [0..7] else [int cylinder - 1 .. int cylinder - 1])

                AnsiConsole.MarkupLine "\n [bold green]Analysis successful![/]\n\n"

                AnsiConsole.Status()
                    .Start("\n\n[bold][slowblink]Loading...[/][/]\n", fun ctx ->
                        Thread.Sleep(200);
                        analysis
                    )
                
                let restart = 
                    AnsiConsole.Confirm(prompt = "\n\n[yellow]Do you want to restart the application?[/]\n\n")
                if restart = true then main argv |> ignore else AnsiConsole.MarkupLine "\n\n Thanks for using my AnalysisTool, i hope you liked it! \n [blue bold]If you want to start again type [underline]dotnet run[/] and press enter.[/] \n\n"

        | _ -> 
            AnsiConsole.MarkupLine "\n\n [red bold]Invalid input.[/] Look into the introduction or see Example above.\n\n"

            let restart = 
                AnsiConsole.Confirm(prompt = "\n\n[yellow]Do you want to try again?[/]\n\n")

            if restart = true then main argv |> ignore else AnsiConsole.MarkupLine "\n\n Thanks for using my AnalysisTool, i hope you liked it! \n [blue bold]If you want to start again type [underline]dotnet run[/] and press enter.[/] \n\n"

    with
    | ex -> 
        let fileName = input.[0]
        let upperThreshold = input.[1] 
        let lowerThreshold = input.[2]
        let cylinder = input.[3]
        
        Thread.Sleep(3000)
        AnsiConsole.MarkupLine "\n\n [red bold]An error occurred:[/] \n  [red]Oh something went wrong, maybe [underline]you mistyped[/] or the data [underline]table layout is not correct[/].[/]\n  [red]Analysis failed.[/]\n\n"

        let restart = 
            AnsiConsole.Confirm(prompt = "\n\n \t[yellow]Do you want to try again?[/]\n\n")

        if restart = true then main argv |> ignore else AnsiConsole.MarkupLine "\n\n Thanks for using my AnalysisTool, i hope you liked it! \n [blue bold]If you want to start again type [underline]dotnet run[/] and press enter.[/] \n\n"
    0