# Chat API (v1/chat/completions)

- [Text generation](#text-generation)
    - [Non streamed](#non-streamed) 


<br>

___

## Text generation

You can send a structured list of input messages containing only text content, and the model will generate the corresponding response message.

The Chat API supports both single‑turn requests and multi‑turn, stateless conversations.

>[!IMPORTANT]
> The async/await methods were introduced on the v1/chat/completion endpoint. Below are two usage examples: <br>
>  - **Non‑streaming response** <br>
>  - **Streaming response**

We assume that you have already unzipped one of the test applications—[TestMistralAI_VCL](https://github.com/MaxiDonkey/DelphiMistralAI/tree/main/sample) or [TestMistralAI_FMX](https://github.com/MaxiDonkey/DelphiMistralAI/tree/main/sample)—so you can integrate the code snippets below with a simple copy and paste.

<br>

### Non streamed

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Chat.AsyncAwaitCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('mistral-tiny');
      Params.Messages([Payload.User('Explain to me what joual is for Quebecers.')]);
      Params.MaxTokens(1024);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<string>(
      function (Value: TChat): string
      begin
        for var Item in Value.Choices do
          Result := Result + Item.Message.Content[0].Text;
        Display(TutorialHub, Value);
        ShowMessage(Result);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.Chat.ASyncCreate(
//    procedure (Params: TChatParams)
//    begin
//      Params.Model('mistral-tiny');
//      Params.Messages([Payload.User('Explain to me what joual is for Quebecers.')]);
//      Params.MaxTokens(1024);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsynChat
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Chat := Client.Chat.Create(
//    procedure (Params: TChatParams)
//    begin
//      Params.Model('mistral-tiny');
//      Params.Messages([Payload.User('Explain to me what joual is for Quebecers.')]);
//      Params.MaxTokens(1024);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(Memo1, Chat);
//  finally
//    Chat.Free;
//  end;
```