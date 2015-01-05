open System
open System.Reflection
open Microsoft.FSharp.Reflection
open System.IO


let testFormatString = """
[<Property>]
let ``json serialize {0}`` item = 
    let serialized = {0}.ToJson item 
    let deserialized = {0}.FromJson item serialized |> ParseResult.get
    item = deserialized
"""

let fileTemplate = """
namespace SerializationTests 

open FsCheck
open FsCheck.Xunit

open Marvel.Json

{0}
"""
let moduleTemplate = """
module {0} =
{1}
"""
let jsonSerializedTypes (asm: Assembly)=
    let collectModules = new System.Collections.Generic.List<string>()
    asm.GetExportedTypes()
    |> Array.filter(fun typ -> 
        let jsonMethods =
            typ.GetMethods()
            |> Array.filter(fun m -> m.Name = "ToJson" || m.Name = "FromJson")
        jsonMethods.Length = 2 )
    |> Array.map(fun t -> 
        if String.IsNullOrEmpty t.Namespace |> not then
            collectModules.Add(t.Namespace)
        if t.DeclaringType <> null && FSharpType.IsModule (t.DeclaringType) then
                collectModules.Add(t.DeclaringType.Name)
        let name = t.Name
        String.Format(testFormatString,name))
    |> (fun tests -> 
        let sb = new System.Text.StringBuilder()
        let modules = collectModules |> Seq.distinct
        for i in modules do 
            sb.AppendFormat("open {0}\n", i)
        sb.AppendLine()
        sb.Append(String.Join ("\n",tests))
        sb.ToString())

let indent spaces (text:string) =
    let indent = new String(' ',spaces)
    let sb = new System.Text.StringBuilder()
    text.Split('\n') |> Array.iter(fun x -> sb.AppendFormat("{0}{1}\n",indent,x) |> ignore)
    sb.ToString()
    
let generateFile root destdir assemblies = 
    assemblies 
    |> List.map (fun dll -> 
        dll,Assembly.LoadFrom(Path.Combine(root,dll)))
    |> List.map (fun (dll,asm) -> 
        let dll = dll.Replace(".dll","")
        let moduleName = dll.Replace(".","")
        let text = jsonSerializedTypes asm |> indent 4
        let testModule = String.Format(moduleTemplate,moduleName,text) 
        let filename = sprintf "%s.JsonTests.fs" dll
        let text = String.Format(fileTemplate,testModule)
        let fullpath = Path.Combine(destdir,filename)
        File.WriteAllText(fullpath,text)
        fullpath )

    
let root = @"C:\dev\thor\Thor.FC\bin\Debug"
let assemblies = [
    "Thor.Core.dll"
    "Thor.FC.dll"
]
let dll = assemblies.[0]
let dir = @"C:\dev\thor\Thor.WebApi.Tests"

generateFile root dir assemblies
