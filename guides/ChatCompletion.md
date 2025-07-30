# Chat API (v1/chat/completions)

- [Text generation](#text-generation)
    - [Non streamed](#non-streamed) 
    - [Streamed](#streamed)
    - [Multi-turn conversations](#multi-turn-conversations) 
    - [Parallel method for generating text](#parallel-method-for-generating-text)
    - [Input Audio for Chat](#input-audio-for-chat)
    - [Vision](#vision)
        - [Analyze single source](#analyze-single-source)
        - [Analyze multi-source](#analyze-multi-source)
    - [Function calling](#function-calling)

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

___

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
      TutorialHub.JSONRequest := Params.ToFormat(); //to display JSON Request
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
//      TutorialHub.JSONRequest := Params.ToFormat(); //to display JSON Request
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
//      TutorialHub.JSONRequest := Params.ToFormat(); //to display JSON Request
//    end);
//  try
//    Display(Memo1, Chat);
//  finally
//    Chat.Free;
//  end;
```

<br>

By using the MistralAI.Tutorial.VCL unit along with the initialization described [above](https://github.com/MaxiDonkey/DelphiMistralAI#strategies-for-quickly-using-the-code-examples), you can achieve results similar to the example shown below.

![Preview](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/sample/ChatCompletionsNS.png?raw=true "Preview")

<br>

___

### Streamed

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;
  
  TutorialHub.JSONResponseClear;

  //Asynchronous promise example
  var Promise := Client.Chat.AsyncAwaitCreateStream(
    procedure(Params: TChatParams)
    begin
      Params.Model('mistral-large-latest');
      Params.Messages([
          Payload.System('You are a literature professor for graduate students and you often mention Jack Kerouac.'),
          Payload.User('Explain to me what joual is for Quebecers.')]);
      Params.MaxTokens(1024);
      Params.Stream;
      TutorialHub.JSONRequest := Params.ToFormat();
    end,
    function : TPromiseChatStream
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;

      Result.OnProgress :=
        procedure (Sender: TObject; Chunk: TChat)
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
//  Client.Chat.ASyncCreateStream(
//    procedure(Params: TChatParams)
//    begin
//      Params.Model('mistral-large-latest');
//      Params.Messages([
//          Payload.System('You are a literature professor for graduate students and you often mention Jack Kerouac.'),
//          Payload.User('Explain to me what joual is for Quebecers.')]);
//      Params.MaxTokens(1024);
//      Params.Stream;
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsynChatStream
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnProgress := DisplayStream;
//      Result.OnSuccess := Display;
//      Result.OnDoCancel := DoCancellation;
//      Result.OnCancellation := Cancellation;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  Client.Chat.CreateStream(
//    procedure(Params: TChatParams)
//    begin
//      Params.Model('mistral-large-latest');
//      Params.Messages([
//          Payload.System('You are a teacher for 8 year old children, you have to adapt your language to your students.'),
//          Payload.User('Explain to me what joual is for Quebecers.')]);
//      Params.MaxTokens(1024);
//      Params.Stream;
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
//    begin
//      if (not IsDone) and Assigned(Chat) then
//        begin
//          DisplayStream(Memo1, Chat);
//        end;
//    end);
```

![Preview](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/sample/ChatCompletionsS.png?raw=true "Preview")

<br>

___

### Multi-turn conversations

The `MistralAI Chat API` enables the creation of interactive chat experiences tailored to your users' needs. Its chat functionality supports multiple rounds of questions and answers, allowing users to gradually work toward solutions or receive help with complex, multi-step issues. This capability is especially useful for applications requiring ongoing interaction, such as:

- **Chatbots**
- **Educational tools**
- **Customer support assistants**

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  //Asynchronous example
  Client.Chat.ASyncCreateStream(
    procedure(Params: TChatParams)
    begin
      Params.Model('mistral-large-latest');
      Params.Messages([
          Payload.System('You are a funny domestic assistant.'),
          Payload.User('Hello'),
          Payload.Assistant('Great to meet you. What would you like to know?'),
          Payload.User('I have two dogs in my house. How many paws are in my house?')
      ]);
      Params.MaxTokens(1024);
      Params.Stream;
      TutorialHub.JSONRequest := Params.ToFormat();
    end,
    function : TAsynChatStream
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnProgress := DisplayStream;
      Result.OnSuccess := Display;
      Result.OnDoCancel := DoCancellation;
      Result.OnCancellation := Cancellation;
      Result.OnError := Display;
    end);

  //Synchronous example
//  Client.Chat.CreateStream(
//    procedure(Params: TChatParams)
//    begin
//      Params.Model('mistral-large-latest');
//      Params.Messages([
//          Payload.System('You are a funny domestic assistant.'),
//          Payload.User('Hello'),
//          Payload.Assistant('Great to meet you. What would you like to know?'),
//          Payload.User('I have two dogs in my house. How many paws are in my house?')
//      ]);
//      Params.MaxTokens(1024);
//      Params.Stream;
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
//    begin
//      if (not IsDone) and Assigned(Chat) then
//        begin
//          DisplayStream(Memo1, Chat);
//        end;
//    end); 
```

<br>

___

### Parallel method for generating text

This approach enables the simultaneous execution of multiple prompts, provided they are all processed by the same model. It also supports parallel web requests.

#### Example : Two prompts processed in parallel.

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  Client.HttpClient.ResponseTimeout := 120000;

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Chat.AsyncAwaitCreateParallel(
    procedure (Params: TBundleParams)
    begin
      Params.Prompts([
        'How many television channels were there in France in 1980?',
        'How many TV channels were there in Germany in 1980?.'
      ]);
      Params.System('Write the response in capital letters.');
      Params.Model('mistral-tiny');
    end);

  Promise
    .&Then<TBundleList>(
      function (Value: TBundleList): TBundleList
      begin
        Result := Value;
        for var Item in Value.Items do
        begin
          Display(TutorialHub, TChat(Item.Chat).Choices[0].Message.Content[0].Text);
        end;
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);


  //Asynchronous example
//  Client.Chat.CreateParallel(
//    procedure (Params: TBundleParams)
//    begin
//      Params.Prompts([
//        'How many television channels were there in France in 1980?',
//        'How many TV channels were there in Germany in 1980?.'
//      ]);
//      Params.System('Write the response in capital letters.');
//      Params.Model('mistral-tiny');
//    end,
//    function : TAsynBundleList
//    begin
//      Result.Sender := TutorialHub;
//
//      Result.OnStart :=
//        procedure (Sender: TObject)
//        begin
//          Display(Sender, 'Start the job' + sLineBreak);
//        end;
//
//      Result.OnSuccess :=
//        procedure (Sender: TObject; Bundle: TBundleList)
//        begin
//          // Background bundle processing
//          for var Item in Bundle.Items do
//            begin
//              Display(Sender, 'Index : ' + Item.Index.ToString);
//              Display(Sender, 'FinishIndex : ' + Item.FinishIndex.ToString);
//              Display(Sender, Item.Prompt + sLineBreak);
//              Display(Sender, Item.Response + sLineBreak + sLineBreak);
//              // or Display(Sender, TChat(Item.Chat).Choices[0].Message.Content);
//            end;
//        end;
//
//      Result.OnError := Display;
//    end);
```

<br>

___

### Input Audio for Chat

It’s possible to include an audio file when calling the `v1/chat/completions` endpoint to process its content within a conversation. You can provide the audio data in two ways: <br>
 - **Via URL (signed or unsigned)**
 - **As a Base64‑encoded string**

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

   var audio_url := 'url, signedurl or string encoded base-64';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Chat.AsyncAwaitCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('voxtral-mini-latest');

      Params.Messages([
        Payload.User('Detail the exchanges in this audio conversation.', audio_url)
      ]);
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
```

<br>

___

### Vision

Refer to the [official documentation](https://docs.mistral.ai/capabilities/vision/).

#### Analyze single source

`MistralAI` processes images from both web sources and local files uniformly. It manages the submission of the source to the API, thereby simplifying the developer's task. Therefore, in this example, we will handle sources in the form of a ***URL*** and ***base-64 encoded*** data.

**Non streamed code example**

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var Ref := '..\..\sample\Invoice.png';
  var Image_url := 'https://assets.visitorscoverage.com/production/wp-content/uploads/2024/04/AdobeStock_626542468-min-1024x683.jpeg';

  //Asynchronous promise example
  var Promise := Client.Chat.AsyncAwaitCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('pixtral-12b-2409');
      Params.Messages([PayLoad.User('Describe the image', [Ref])]);
      //or 
      // Params.Messages([PayLoad.User('Describe the image', [Ref])]);
      Params.MaxTokens(1024);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  Promise
    .&Then<TChat>(
      function (Value: TChat): TChat
      begin
        Result := Value;
        Display(TutorialHub, Value);
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
//      Params.Model('pixtral-12b-2409');
//      Params.Messages([PayLoad.User('Describe the image', [Ref])]);
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
//  var Vision := Client.Chat.Create(
//    procedure (Params: TChatParams)
//    begin
//      Params.Model('pixtral-12b-2409');
//      Params.Messages([PayLoad.User('Describe the image', [Ref])]);
//      Params.MaxTokens(1024);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, Vision);
//  finally
//    Vision.Free;
//  end;
```

#### Analyze multi-source

**Streamed code example**

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var Ref := '..\..\sample\Invoice.png';
  var Image_url := 'https://assets.visitorscoverage.com/production/wp-content/uploads/2024/04/AdobeStock_626542468-min-1024x683.jpeg';

  //Asynchronous promise example
  var Promise := Client.Chat.AsyncAwaitCreateStream(
    procedure (Params: TChatParams)
    begin
      Params.Model('pixtral-12b-2409');
      Params.Messages([PayLoad.User('What difference between images.', [Ref, Image_url])]);
      Params.MaxTokens(1024);
      Params.Stream;
      TutorialHub.JSONRequest := Params.ToFormat();
    end,
    function : TPromiseChatStream
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;

      Result.OnProgress :=
        procedure (Sender: TObject; Chunk: TChat)
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
//  Client.Chat.ASyncCreateStream(
//    procedure (Params: TChatParams)
//    begin
//      Params.Model('pixtral-12b-2409');
//      Params.Messages([PayLoad.User('What difference between images.', [Ref, Image_url])]);
//      Params.MaxTokens(1024);
//      Params.Stream;
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsynChatStream
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnProgress := DisplayStream;
//      Result.OnSuccess := Display;
//      Result.OnDoCancel := DoCancellation;
//      Result.OnCancellation := Cancellation;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  Client.Chat.CreateStream(
//    procedure (Params: TChatParams)
//    begin
//      Params.Model('pixtral-12b-2409');
//      Params.Messages([PayLoad.User('What difference between images.', [Ref, Image_url])]);
//      Params.MaxTokens(1024);
//      Params.Stream;
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
//    begin
//      if (not IsDone) and Assigned(Chat) then
//        begin
//          DisplayStream(Memo1, Chat);
//        end;
//    end);
```

<br>

___

### Function calling

Allow models to access data and execute actions. <br/>
Function calling offers a robust and versatile method for MistralAI models to interact with your code or external services, serving two main purposes:

- **Data Retrieval:** Access real-time information to enhance the model's responses (RAG). This is particularly beneficial for searching knowledge bases and extracting specific data from APIs (e.g., obtaining the current weather).

- **Action Execution:** Carry out tasks such as form submissions, API calls, updating the application state (UI/frontend or backend), or executing agent-driven workflows (e.g., transferring a conversation).

Refer to the [official documentation](https://docs.mistral.ai/capabilities/function_calling/).

<br>

#### How build a plugin

Use case : **What’s the weather in Paris?**

In the `MistralAI.Functions.Example` unit, there is a class that defines a function which DelphiMistralAI can choose to use or not, depending on the options provided. This class inherits from a parent class defined in the `MistralAI.Functions.Core` unit. To create new functions, you can derive from the `TFunctionCore` class and define a new plugin.

<br>

#### Plan
 - [Use a schema](#use-a-schema)
 - [Methods to display result](#methods-to-display-result)
 - [Display a stream text](#display-a-stream-text) 
 - [The main method](#the-main-method)
 - [The FinishReason](#the-finishreason)

<br>

#### Use a schema

In this unit, this schema will be used for function calls.
```Json
{
    "type": "object",
    "properties": {
         "location": {
             "type": "string",
             "description": "The city and department, e.g. Marseille, 13"
         },
         "unit": {
             "type": "string",
             "enum": ["celsius", "fahrenheit"]
         }
     },
     "required": ["location"],
     "additionalProperties": false
}
```

<br/>

We will use the TWeatherReportFunction plugin defined in the `MistralAI.Functions.Example` unit.

```Delphi
  var Weather := TWeatherReportFunction.CreateInstance;
  //or
  var Weather := TWeatherReportFunction.CreateInstance(True);  //To activate `Strict` option

  //See step : Main method
```
<br/>

#### Methods to display result

We then define a method to display the result of the query using the Weather tool.

With this tutorial, a method is defined within TutorialHub. Let’s take a closer look at how this method works.

<br>

##### Display a stream text

```Delphi
procedure TVCLTutorialHub.WeatherFunction(const Value: TCalledFunction;
  Func: IFunctionCore);
begin
  var ArgResult := Func.Execute(Value.&Function.Arguments);

  FClient.Chat.AsyncCreateStream(
    procedure (Params: TChatParams)
    begin
      Params.Model('open-mixtral-8x22b-2404');
      Params.Messages([
        PayLoad.System('Respond like a star weather presenter on a prime-time TV channel.'),
        Payload.User(ArgResult)
      ]);
      Params.Stream(True);
      Params.MaxTokens(1024);
    end,
    function : TAsynChatStream
    begin
      Result.Sender := TutorialHub;
      Result.OnProgress := DisplayStream;
      Result.OnSuccess := Display;
      Result.OnDoCancel := DoCancellation;
      Result.OnCancellation := Cancellation;
      Result.OnError := Display;
    end);
end;  
```

>[!NOTE]
> The same method also exists with the FMX version of the TutorialHub wizard.

<br/>

#### The main method

Building the query using the Weather tool. (Simply copy/paste this last code to test the usage of the functions.)

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL, MistralAI.Functions.Example or Mistral.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var WeatherFunc: IFunctionCore := TWeatherReportFunction.Create;

  TutorialHub.Tool := WeatherFunc;
  TutorialHub.ToolCall := TutorialHub.WeatherFunction;

  //Asynchronous example
  Client.Chat.ASyncCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('mistral-large-latest');
      Params.Messages([TChatMessagePayload.User('What''s the weather like in Paris?')]);
      Params.Tools([TChatMessageTool.Add(WeatherFunc)]);
      Params.ToolChoice(TToolChoice.auto);
      Params.MaxTokens(1024);
      TutorialHub.JSONRequest := Params.ToFormat();
    end,
    function : TAsynChat
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);

  //Synchronous example
//  var Chat := Client.Chat.Create(
//    procedure (Params: TChatParams)
//    begin
//      Params.Model('mistral-large-latest');
//      Params.Messages([TChatMessagePayload.User('What''s the weather like in Paris?')]);
//      Params.Tools([TChatMessageTool.Add(WeatherFunc)]);
//      Params.ToolChoice(auto);
//      Params.MaxTokens(1024);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, Chat);
//  finally
//    Chat.Free;
//  end;
``` 

<br>

#### The FinishReason

Let's look at how the display method handles the function call.

```Delphi
procedure Display(Sender: TObject; Value: TChat);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Choices do
    if Item.FinishReason = TFinishReason.tool_calls then
      begin
        if Assigned(TutorialHub.ToolCall) then
          TutorialHub.ToolCall(Item.Message.ToolsCalls[0], TutorialHub.Tool);
      end
    else
      begin
        Display(Sender, Item.Message.Content[0].Text);
      end;
end;
```

>[!CAUTION]
>Ensure user confirmation for actions like sending emails or making purchases to avoid unintended consequences.

<br/>