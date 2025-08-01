# Fill-in-the-middle (v1/fim/completions)

- [Code generation](#code-generation)
    - [Before using](#before-using)
    - [Codestral initialization](#codestral-initialization)
- [Code completion](#code-completion)
    - [Streamed Code completion](#streamed-code-completion)
- [Fill in the middle](#fill-in-the-middle)
- [Stop tokens](#stop-tokens)

<br>

___

## Code generation

**Codestral** is an advanced generative model optimized for code generation, including **fill-in-the-middle** and code completion. Trained on over 80 programming languages, it performs well on both common and rare languages.
See also [Code generation](https://docs.mistral.ai/capabilities/code_generation/) at the MistralAI web site.

<br/>

___

### Before using

To utilize the Delphi classes managing the **Codestral** function, you are required to create a new KEY on the ***Mistral.ai website***. Please note that obtaining this key necessitates providing a valid phone number. 
Go to this address to create a key for using **Codestral** [Key creation](https://console.mistral.ai/codestral)

<br/>

___

### Codestral initialization

When instantiating the interface managing the ***TMistralAI*** type class, the `CodestralSpec` specification must be specified in the `create` constructor.

The resulting interface will handle both **CodeStral** functionality as well as chat-type interactions.

```Pascal
uses MistralAI;

var CodingModel: IMistralAI := TMistralAI.Create(API_TOKEN, [CodestralSpec]);
```

You can also use the factory:
> [!NOTE]
> ```Pascal
> uses MistralAI;
>
> var CodingModel := TMistralAIFactory.CreateInstance(API_TOKEN, [CodestralSpec]);
> ```

<br/>

___

## Code completion

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  TutorialHub.Memo2.Text := CodeBefore;
  Start(TutorialHub);

  //Asynchronous promise example
  var Promise := ClientCoding.Codestral.AsyncAwaitCreate(
    procedure (Params: TCodestralParams)
    begin
      Params.Model('codestral-latest');
      Params.Prompt(CodeBefore);
      Params.MaxTokens(1024);
      Params.Stop(['\n\n']);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<string>(
      function (Value: TCodestral): string
      begin
        for var Item in Value.Choices do
          Result := Result + Item.Message.Content;
        Display(TutorialHub, Value);
        ShowMessage(Result);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  ClientCoding.Codestral.AsyncCreate(
//    procedure (Params: TCodestralParams)
//    begin
//      Params.Model('codestral-latest');
//      Params.Prompt(CodeBefore);
//      Params.MaxTokens(1024);
//      Params.Stop(['\n\n']);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsynCode
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Codestral := ClientCoding.Codestral.Create(
//    procedure (Params: TCodestralParams)
//    begin
//      Params.Model('codestral-latest');
//      Params.Prompt(CodeBefore);
//      Params.MaxTokens(1024);
//      Params.Stop(['\n\n']);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, CodeStral);
//  finally
//    Codestral.Free;
//  end;
```

<br/>

___

### Streamed Code completion

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONResponseClear;
  Memo2.Text := string.Join(sLineBreak, [CodeBefore, '//Insert code here' + sLineBreak, CodeAfter]);

  //Asynchronous promise example
  var Promise := ClientCoding.Codestral.AsyncAwaitCreateStream(
    procedure(Params: TCodestralParams)
    begin
      Params.Model('codestral-latest');
      Params.Prompt(CodeBefore);
      Params.Suffix(CodeAfter);
      Params.MaxTokens(1024);
      Params.Stream;
      TutorialHub.JSONRequest := Params.ToFormat();
    end,
    function : TPromiseCodeStream
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;

      Result.OnProgress :=
        procedure (Sender: TObject; Chunk: TCodestral)
        begin
          DisplayStream(Sender, Chunk);
        end;

      Result.OnDoCancel := DoCancellation;

      Result.OnCancellation :=
        function (Sender: TObject): string
        begin
          Cancellation(Sender);
        end
    end);

  Promise
    .&Then<string>(
      function (Value: string): string
      begin
        Result := Value;
        ShowMessage(Result);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  ClientCoding.Codestral.ASyncCreateStream(
//    procedure(Params: TCodestralParams)
//    begin
//      Params.Model('codestral-latest');
//      Params.Prompt(CodeBefore);
//      Params.MaxTokens(1024);
//      Params.Stream;
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsynCodeStream
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnProgress := DisplayStream;
//      Result.OnDoCancel := DoCancellation;
//      Result.OnCancellation := Cancellation;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  ClientCoding.Codestral.CreateStream(
//    procedure(Params: TCodestralParams)
//    begin
//      Params.Model('codestral-latest');
//      Params.Prompt(CodeBefore);
//      Params.MaxTokens(1024);
//      Params.Stream;
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    procedure(var Code: TCodestral; IsDone: Boolean; var Cancel: Boolean)
//    begin
//      if (not IsDone) and Assigned(Code) then
//        begin
//          DisplayStream(TutorialHub, Code);
//        end
//    end);
```

<br/>

___

## Fill in the middle

This feature allows users to set the beginning of their code with a `prompt` and to specify the end of the code using an optional `suffix` and an optional `stop` condition. The Codestral model will then produce the code that seamlessly fits between these markers, making it perfect for tasks that need a particular segment of code to be created.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONResponseClear;
  Memo2.Text := string.Join(sLineBreak, [CodeBefore, '//Insert code here' + sLineBreak, CodeAfter]);

  //Asynchronous promise example
  var Promise := ClientCoding.Codestral.AsyncAwaitCreateStream(
    procedure(Params: TCodestralParams)
    begin
      Params.Model('codestral-latest');
      Params.Prompt(CodeBefore);
      Params.Suffix(CodeAfter);
      Params.Stop(['\n\n']);
      Params.MaxTokens(1024);
      Params.Stream;
      TutorialHub.JSONRequest := Params.ToFormat();
    end,
    function : TPromiseCodeStream
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;

      Result.OnProgress :=
        procedure (Sender: TObject; Chunk: TCodestral)
        begin
          DisplayStream(Sender, Chunk);
        end;

      Result.OnDoCancel := DoCancellation;

      Result.OnCancellation :=
        function (Sender: TObject): string
        begin
          Cancellation(Sender);
        end
    end);

  Promise
    .&Then<string>(
      function (Value: string): string
      begin
        Result := Value;
        ShowMessage(Result);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
```

The model will create the intermediate code completing the codes provided to the `prompt` and `suffix` parameters.

<br/>

___

## Stop tokens

It is advisable to include stop tokens when integrating with IDE autocomplete to ensure the model doesn't provide overly verbose output.

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.Memo2.Text := string.Join(sLineBreak, [CodeBefore, '//Insert code here'+sLineBreak, CodeAfter]); // For tutorial tool use only

  CodingModel.Codestral.ASyncCreateStream(
    procedure(Params: TCodestralParams)
    begin
      Params.Model('codestral-latest');
      Params.Prompt(CodeBefore);
      Params.Suffix(CodeAfter);
      Params.MaxTokens(1024);
      Params.Stream;
      Params.Stop(['\n\n']); // <-- Include here the strings responsible for stopping processing
    end,
    function : TAsynCodeStream
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnProgress := DisplayStream;
      Result.OnDoCancel := DoCancellation;
      Result.OnCancellation := Cancellation;
      Result.OnError := Display;
    end); 
```
