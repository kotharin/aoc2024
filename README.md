# aoc2024

## Project Creation
dotnet new console -lang "F#" -n aoc2024

## Add xunit
dotnet add package Microsoft.NET.Test.Sdk
dotnet add package xunit
dotnet add package xunit.runner.visualstudio

### Run Tests
dotnet test aoc2024.fsproj --logger:"console;verbosity=normal"