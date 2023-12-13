namespace Library

    type tableRow =
        {
            PhaseID : int;
            startTimeGrowphase : float;
            endTimeGrowphase : float;
            slopeOrGrowrateOfLinearRegressionOrGrowphase : float;
            duplicationTimeOfGrowphase : float
        }

    type Result<'a, 'b> =
        | Ok of 'a
        | Error of 'b
