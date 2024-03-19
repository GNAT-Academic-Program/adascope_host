# UI

## Outline

- [Team Members and their Roles](#team-members-and-their-roles)
- [Related Repo](#related-repo)
- [Documentation](#documentation)
- [Objectives](#objectives)
- [Expected/Anticipated Architecture](#expectedanticipated-architecture)
- [Anticipated Risks](#anticipated-risks)
- [Legal and Social Issues](#legal-and-social-issues)
- [Initial Plans for Release](#initial-plans-for-release)

## Team Members and their Roles

| Team Member           |            Role            |
| --------------------- | :------------------------: |
| Florian Koudjonou     | Scrum Master and developer |
| Anne-Sophie Cusson    |         Developer          |
| Jean-Sébastien Demers |         Developer          |
| Basma Kaanane         |         Developer          |

These roles may evolve or change throughout the project, but we have decided not to start the project with very strict roles.

## Related Repo
[Firmware repository](https://github.com/AdaScope/firmware)

## Documentation

### [Main documentation location](https://adascope.atlassian.net/jira/software/projects/ADASCOPE/pages)

### [Meeting minutes and detailed meeting notes](https://adascope.atlassian.net/wiki/spaces/DOCUMENTAT/pages/917507/Meeting+notes)

### [Competitive analysis, vision and conception document](https://adascope.atlassian.net/wiki/spaces/DOCUMENTAT/pages/4128769/Competitive+analysis+vision+and+conception+document)

### [Requirements](https://adascope.atlassian.net/wiki/spaces/DOCUMENTAT/pages/1048577/Requirements)

### [Tutorials and procedures](https://adascope.atlassian.net/wiki/spaces/DOCUMENTAT/pages/950274/Tutorials)

### [Resources from AdaCore engineer](https://adascope.atlassian.net/wiki/spaces/DOCUMENTAT/pages/2916354/Resources+-+Olivier+Henley)


## Objectives
#### Benefit to Customer
Oscilloscopes are specifically designed for waveform measurement and typically are faster and more precise than other types of testing devices widely found on the market. The oscilloscope developped will allow the customer to perform various amplitude and frequence measurements accurately and fastly. Furthermore, it is going to show the different signals curves and shapes on its interface which will help the customer understand the behavior and nature of signals. 
#### Key Things to Accomplish
The oscilloscope have to : 
    - calculate a signal amplitude plus the signal frequency
    - display if the signal is of the expected shape 
    - diplay shapes within a grid overlay
    - provide the ability to control and change how many volts are represented by each vertical increment of grid overlay on the screen.
    - measure the A.C. signals
    - Show the maximum and minimal signal amplitude 
#### Criteria for success
The oscilloscope should : 
    - be able to calculate a signal amplitude and frequency with accuracy
    - be available to use when needed
    - display the signal shape in its interface with accuracy 
    - have a bandwidth that is superior than the highest frequency component signal multiplied by 5 
    - be user friendly 
    - respect the safety and security norms 
    - The oscilloscope code have to be easily maintainable 

## Expected/Anticipated Architecture

We will build the entire firmware of the oscilloscope using Ada and SPARK. Our client wants us to use those two languages for their reliability.
That firmware will then be executed on an STM32MP157 board running OpenSTLinux.
The signals measured by the board will then be sent to another computer via UART.
That second computer will run another program for the UI. That program will also be written in Ada and/or SPARK and we will use a library called GtkAda for the UI.

## Anticipated Risks

The main risks anticipated for the development of the oscilloscope are: inaccuracy of results since a lot of calculations would be needed - several breakdown of the oscilloscope may happen since the oscilloscope should be available whenever needed - lack of perfomance of the UI since we have strict time constraints 

## Legal and Social Issues

The main legal or social issues associated with our project would be reliability.

Oscilloscopes are used to perform tests and maintenance on devices in many fields where the reliability of the equipment is top priority. These industries include aerospace & defense, civil aviation, rail, and medical industries. An unreliable oscilloscope used in those industries could be responsible for life threatening incidents. Therefore, the highest standards of reliability, accuracy and quality must be met by the AdaScope Oscilloscope.

## Initial Plans for Release

We currently have two github repositories for the project. We have the ui repo which holds the software that will run the user interface on the host pc, and the firmware repo that will do data collection on the stm32mp157f-dk2 development board.

As we are all new to ADA and the development board, we will start by writing small test programs to get comfortable with the technologies. Afterwards, we will start working on the ui and firmware modules separately. For the first release of the ui module, we will likely feed the graphical interface dummy data. Same idea for the firmware module, we will make the board collect data without sending it anywhere. Once we have made good progress on both modules, we will work on making them communicate with each other.

Our goal is to make a basic oscilloscope. Once that goal is achieved, we will work on additional features.

In terms of tool setup, the host pc will be running linux (likely ubuntu) and the development board will be running linux as well (version tbd). We also need to install an ada compiler on both devices (gnat).
