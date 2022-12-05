.PHONY: all
all: restore cleanup build

cleanup:
	rm -f '.\contract\FSharp.Avro.Apache.Contract\generated\simple.avsc.fs'

restore:
	dotnet tool restore
	dotnet restore

build:
	dotnet build --no-restore '.\src\FSharp.Avro.Apache.Codegen\FSharp.Avro.Apache.Codegen.fsproj'
	dotnet build --no-restore -v n

