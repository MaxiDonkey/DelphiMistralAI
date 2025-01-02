# Delphi MistralAI API

___
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.3/11/12-yellow)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-green)
![GitHub](https://img.shields.io/badge/Updated%20on%20january%203,%202025-blue)

<br/>
<br/>

- [Introduction](#Introduction)
- [Changelog](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/Changelog.md)
- [Remarks](#remarks)
- [Wrapper Tools Info](#Wrapper-Tools-Info)
    - [Tools for simplifying this tutorial](#Tools-for-simplifying-this-tutorial)
    - [Asynchronous callback mode management](#Asynchronous-callback-mode-management)
    - [Simplified Unit Declaration](#Simplified-Unit-Declaration) 
- [Usage](#usage)
    - [Initialization](#initialization)
    - [Models](#models)
        - [Retrieve the list of models](#Retrieve-the-list-of-models)
        - [Retrieve Model](#Retrieve-Model)
        - [Fine tuned Models](#Fine-tuned-Models)
    - [Embeddings](#embeddings)
    - [Chats](#chats)
        - [Synchronous](#Synchronous)
            - [Not Streamed](#Not-Streamed)
            - [Streamed](#Streamed)
        - [Asynchronous](#Asynchronous)
            - [Asynchronous Not Streamed](#Asynchronous-Not-Streamed)
            - [Asynchronous Streamed](#Asynchronous-Streamed)
    - [Vision](#vision)
        - [Passing an Image URL](#Passing-an-image-url)
        - [Passing a Base64 Encoded Image](#Passing-a-base64-encoded-image)	
    - [Function calling](#function-calling)
        - [The weather in Paris](#The-weather-in-Paris)
    - [JSON mode](#JSON-mode)
    - [Code generation](#Code-generation)
        - [Before using](#Before-using)
        - [Codestral initialization](#Codestral-initialization)
        - [Code completion](#Code-completion)
        - [Streamed Code completion](#Streamed-code-completion)
        - [Fill in the middle](#Fill-in-the-middle)
        - [Stop tokens](#Stop-tokens)
        - [End points](#End-points)
    - [Files](#Files)
        - [List of files](#[List-of-files)
        - [File Retrieve](#File-Retrieve)
        - [File Upload](#File-Upload)
        - [File Delete](#File-Delete)
        - [File Download](#File-Download)
    - [Fine-tuning](#Fine-tuning)
        - [Create a Fine-tuning Job](#Create-a-Fine-tuning-Job)
        - [Delete a Fine-tuned Model](#Delete-a-Fine-tuned-Model)
        - [List of Fine-tune Job](#List-of-Fine-tune-Job)
        - [Retrieve a Fine-tune Job](#Retrieve-a-Fine-tune-Job)
        - [Start a Fine-tune Job](#Start-a-Fine-tune-Job)
        - [Cancel a Fine-tune Job](#Cancel-a-Fine-tune-Job)
    - [Moderation](#Moderation)
        - [Raw-text endpoint](#Raw-text-endpoint)
        - [Conversational endpoint](#Conversational-endpoint)
    - [Batch Inference](#Batch-Inference)
        - [Batch List](#Batch-List)
        - [Batch Job Create](#Batch-Job-Create)
        - [Batch Job Cancel](#Batch-Job-Cancel)
        - [Batch Job Retrieve](#Batch-Job-Retrieve)
        - [Batch Job Result File](#Fatch-Job-Result-File)
    - [Agents](#Agents)
- [Contributing](#contributing)
- [License](#license)

<br/>
<br/>


# Introduction

Welcome to the Unofficial **Delphi MistralAI API Library**
This library is designed to provide a seamless `Delphi` interface for interacting with the `MistralAI public API`. It simplifies the integration of advanced natural language processing capabilities into your `Delphi` applications. Whether your goal is text generation, creating embeddings, leveraging chat models, managing batch jobs, performing text evaluation with classifiers for moderation, or generating code, this library offers a streamlined and efficient solution to meet your needs.

**MistralAI** is a powerful natural language processing API that enables developers to incorporate advanced AI functionalities into their applications. For more details, visit the [official MistralAI documentation](https://docs.mistral.ai/api/).

<br/>

# Remarks 

> [!IMPORTANT]
>
> This is an unofficial library. **MistralAI** does not provide any official library for `Delphi`.
> This repository contains `Delphi` implementation over [MistralAI](https://docs.mistral.ai/api/) public API.

<br/>

# Wrapper Tools Info

This section provides brief notifications and explanations about the tools available to simplify the presentation and understanding of the wrapper's functions in the tutorial.

<br>

## Tools for simplifying this tutorial

To streamline the code examples provided in this tutorial and facilitate quick implementation, two units have been included in the source code: `MistralAI.Tutorial.VCL` and `MistralAI.Tutorial.FMX`. Depending on the platform you choose to test the provided source code, you will need to instantiate either the `TVCLTutorialHub` or `TFMXTutorialHub` class in the application's OnCreate event, as demonstrated below:

>[!TIP]
>```Pascal
> //uses MistralAI.Tutorial.VCL;
> TutorialHub := TVCLTutorialHub.Create(Memo1, Memo2, Button1);
>```


or

>[!TIP]
>```Pascal
> //uses MistralAI.Tutorial.FMX;
> TutorialHub := TFMXTutorialHub.Create(Memo1, Memo2, Button1);
>```

Make sure to add two `TMemo` and a `TButton` component to your form beforehand.

The `TButton` will allow the interruption of any streamed reception.

<br/>

## Asynchronous callback mode management

In the context of asynchronous methods, for a method that does not involve streaming, callbacks use the following generic record: `TAsyncCallBack<T> = record` defined in the `MistralAI.Async.Support.pas` unit. This record exposes the following properties:

```Pascal
   TAsyncCallBack<T> = record
   ... 
       Sender: TObject;
       OnStart: TProc<TObject>;
       OnSuccess: TProc<TObject, T>;
       OnError: TProc<TObject, string>; 
```
<br/>

For methods requiring streaming, callbacks use the generic record `TAsyncStreamCallBack<T> = record`, also defined in the `MistralAI.Async.Support.pas` unit. This record exposes the following properties:

```Pascal
   TAsyncCallBack<T> = record
   ... 
       Sender: TObject;
       OnStart: TProc<TObject>;
       OnSuccess: TProc<TObject>;
       OnProgress: TProc<TObject, T>;
       OnError: TProc<TObject, string>;
       OnCancellation: TProc<TObject>;
       OnDoCancel: TFunc<Boolean>;
```

The name of each property is self-explanatory; if needed, refer to the internal documentation for more details.

>[!NOTE]
> All methods managed by the wrapper are designed to support both synchronous and asynchronous execution modes. This dual-mode functionality ensures greater flexibility for users, allowing them to choose the approach that best suits their application's requirements and workflow.

<br/>

## Simplified Unit Declaration

To streamline the use of the API wrapper, the process for declaring units has been simplified. Regardless of the methods being utilized, you only need to reference the following two core units:

```Pascal
  uses
    MistralAI, MistralAI.Types;
```

If required, you may also include the `MistralAI.Schema` unit or any plugin units developed for specific function calls (e.g., `MistralAI.Functions.Example`). This simplification ensures a more intuitive and efficient integration process for developers.

<br/>

# Usage

## Initialization

To initialize the API instance, you need to [obtain an API token from MistralAI](https://console.mistral.ai/api-keys/).

Once you have a token, you can initialize `IMistralAI` interface, which is an entry point to the API.

```Pascal
uses MistralAI;

var MistralAI := TMistralAIFactory.CreateInstance(API_TOKEN);
```

<br/>

When instantiating the interface managing the TMistralAI type class, the CodestralSpec specification can be specified in the create constructor.

The resulting interface will handle both `CodeStral` functionality as well as chat-type interactions.

```Pascal
uses MistralAI;

var CodingModel := TMistralAIFactory.CreateInstance(API_TOKEN, [CodestralSpec]);
```

>[!Warning]
> To use the examples provided in this tutorial, especially to work with asynchronous methods, I recommend defining the `MistralAI` and `CodingModel`  interfaces with the widest possible scope.
> <br/>
> So, set 
> - `MistralAI := TMistralAIFactory.CreateInstance(API_TOKEN);`  and 
> - `CodingModel := TMistralAIFactory.CreateInstance(API_TOKEN, [CodestralSpec]);`  
> in the `OnCreate` event of your application.
> <br/>
> Having previously declared  MistralAI: IMistralAI; and CodingModel: IMistralAI;
>

<br/>

## Models

The `Model` unit manages not only the models provided by `MistralAI` but also those that have been fine-tuned for specific use cases. This flexibility ensures seamless integration and utilization of both pre-trained and customized models.

For more details on the available models, please refer to the [Models Documentation](https://docs.mistral.ai/models/)

### Retrieve the list of models

**Synchronously code example :**

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  var Models := MistralAI.Models.List;
    try
      Display(TutorialHub, Models);
    finally
      Models.Free;
    end;
```

**Asynchronously code example :**

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  MistralAI.Models.AsyncList(
    function : TAsynModels
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```
<br/>

### Retrieve Model

**Synchronously code example :**

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  var Model := MistralAI.Models.Retrieve('mistral-large-2407');
  try
    Display(TutorialHub, Model);
  finally
    Model.Free;
  end;
```

**Asynchronously code example :**

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  MistralAI.Models.AsyncRetrieve('mistral-large-2407',
    function : TAsynModel
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

<br/>

### Fine tuned Models

Fine-tuned models can be managed through various operations, including deletion, renaming, archiving, and unarchiving. To perform these actions, the following functions are available in both synchronous and asynchronous modes:

**Synchronous Mode:** 
```Pascal
- function Delete(const ModelId: string): TModelDeletion;
- function Update(const ModelId: string; ParamProc: TProc<TModelParams>): TFineTunedModel;
- function Archive(const ModelId: string): TArchivingModel;
- function Unarchive(const ModelId: string): TArchivingModel;
```

**Asynchronous Mode:** 
```Pascal
- procedure AsyncDelete(const ModelId: string; const CallBacks: TFunc<TAsynModelDeletion>);
- procedure AsyncUpdate(const ModelId: string; ParamProc: TProc<TModelParams>;
      const CallBacks: TFunc<TAsynFineTuneModel>);
- procedure AsyncArchive(const ModelId: string; const CallBacks: TFunc<TAsynArchivingModel>);
- procedure AsyncUnarchive(const ModelId: string; const CallBacks: TFunc<TAsynArchivingModel>);
```

These functions provide a flexible and comprehensive approach to maintaining and organizing fine-tuned models.

<br>

## Embeddings

Embeddings make it possible to vectorize one or more texts in order, for example, to calculate the similarity between sentences. Each vector resulted will be of dimension 1024. This vector representation captures deep semantic aspects of texts, allowing for more nuanced comparisons.
Distance measures such as cosine, Euclidean distance or other custom measures can be applied to these embeddings. 

See also [tokenization](https://docs.mistral.ai/guides/tokenization/) at the MistralAI web site.

**Synchronously code example :**

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  var Embeddings := MistralAI.Embeddings.Create(
    procedure (Params: TEmbeddingParams)
    begin
      Params.Input(['aba', 'bbb']);
    end);
  try
    Display(TutorialHub, Embeddings);
  finally
    Embeddings.Free;
  end;
```

<br/>

**Asynchronously**, we proceed as follows:

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  MistralAI.Embeddings.AsyncCreate(
    procedure (Params: TEmbeddingParams)
    begin
      Params.Input([ 'Text to vectorize' ]);
    end,
    function : TAsyncEmbeddings
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);  
```

<br/>

## Chats

Using the API to create and maintain conversations. You have the option to either wait for a complete response or receive the response sequentially (Streaming mode).

See also [Prompting Capabilities](https://docs.mistral.ai/guides/prompting_capabilities/) at the MistralAI web site.

<br/>

### Synchronous

#### Not Streamed

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  var Chat := MistralAI.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params.Model('mistral-tiny');
      Params.Messages([Payload.User('Explain to me what joual is for Quebecers.')]);
      Params.MaxTokens(1024);
    end);
  try
    Display(Memo1, Chat);
  finally
    Chat.Free;
  end;
```

<br/>

#### Streamed

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  MistralAI.Chat.CreateStream(
    procedure(Params: TChatParams)
    begin
      Params.Model('mistral-large-latest');
      Params.Messages([
          Payload.System('You are a teacher for 8 year old children, you have to adapt your language to your students.'),
          Payload.User('Explain to me what joual is for Quebecers.')]);
      Params.MaxTokens(1024);
      Params.Stream;
    end,
    procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
    begin
      if (not IsDone) and Assigned(Chat) then
        begin
          DisplayStream(Memo1, Chat);
        end;
    end);
```

<br/>

### Asynchronous

You can use asynchronous methods for text completion or chat tasks. For this, you need to use the two methods

 1. `procedure AsyncCreate(ParamProc: TProc<TChatParams>; Events: TFunc<TAsynChat>)`

 2. `procedure AsyncCreateStream(ParamProc: TProc<TChatParams>; Events: TFunc<TAsynChatStream>)`

as follows :

#### Asynchronous Not Streamed

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  MistralAI.Chat.ASyncCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('mistral-tiny');
      Params.Messages([Payload.User('Explain to me what joual is for Quebecers.')]);
      Params.MaxTokens(1024);
    end,
    function : TAsynChat
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);  
```
<br/>

#### Asynchronous Streamed


```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  MistralAI.Chat.ASyncCreateStream(
    procedure(Params: TChatParams)
    begin
      Params.Model('mistral-large-latest');
      Params.Messages([
          Payload.System('You are a literature professor for graduate students and you often mention Jack Kerouac.'),
          Payload.User('Explain to me what joual is for Quebecers.')]);
      Params.MaxTokens(1024);
      Params.Stream;
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
```

<br/>

## Vision

The latest Pixtral 12B adds vision capabilities, allowing to analyze both images and text, expanding its potential for applications requiring multimodal understanding.
See also [official documentation](https://docs.mistral.ai/capabilities/vision/).

To support both synchronous and asynchronous completion methods, we focused on generating the appropriate payload for message parameters. An overloaded version of the `TChatMessagePayload.User` class function was added, allowing users to include a dynamic array of text elements—either URLs or file paths—alongside the user's text content. Internally, this data is processed to ensure the correct operation of the vision system in both synchronous and asynchronous contexts.

<br/>

### Passing an Image URL

**Synchronously code example :**

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  // png, jpeg, gif and webp formats are supported
  var Ref1 := 'https://tripfixers.com/wp-content/uploads/2019/11/eiffel-tower-with-snow.jpeg';
  var Ref2 := 'https://assets.visitorscoverage.com/production/wp-content/uploads/2024/04/AdobeStock_626542468-min-1024x683.jpeg';

  var Vision := MistralAI.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params.Model('pixtral-12b-2409');
      Params.Messages([Payload.User('what are the differences between two images?', [Ref1, Ref2])]);
      Params.MaxTokens(1024);
    end);
  try
    Display(TutorialHub, Vision);
  finally
    Vision.Free;
  end;
```

**Asynchronously code example :**

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  // png, jpeg, gif and webp formats are supported
  var Ref1 := 'https://tripfixers.com/wp-content/uploads/2019/11/eiffel-tower-with-snow.jpeg';
  var Ref2 := 'https://assets.visitorscoverage.com/production/wp-content/uploads/2024/04/AdobeStock_626542468-min-1024x683.jpeg';

  MistralAI.Chat.ASyncCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('pixtral-12b-2409');
      Params.Messages([Payload.User('what are the differences between two images?', [Ref1, Ref2])]);
      Params.MaxTokens(1024);
    end,
    function : TAsynChat
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

<br/>

### Passing a Base64 Encoded Image

**Synchronously code example :**

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  // png, jpeg, gif and webp formats are supported
  var Ref := 'D:\My_folder\Images\my_image.png';  

  var Vision := MistralAI.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params.Model('pixtral-12b-2409');
      Params.Messages([Payload.User('Describe the image', [Ref])]);
      Params.MaxTokens(1024);
    end);
  try
    Display(TutorialHub, Vision);  
  finally
    Vision.Free;
  end;  
```

**Asynchronously code example :**

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  MistralAI.Chat.ASyncCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('pixtral-12b-2409');
      Params.Messages([PayLoad.User('Describe the image', [Ref])]);
      Params.MaxTokens(1024);
    end,
    function : TAsynChat
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

>[!NOTE]
> Since the goal is to use the chat API, you can adapt these examples for scenarios involving streamed responses.

<br/>

## Function calling

Function calling allows Mistral models to connect to external tools. By integrating Mistral models with external tools such as user defined functions or APIs, users can easily build applications catering to specific use cases and practical problems. 

See also [documentation](https://docs.mistral.ai/capabilities/function_calling/) at the MistralAI web site.

Warning : While this technology is powerful, it also carries potential risks. We strongly advise incorporating user confirmation processes before executing real-world actions on behalf of users, such as sending emails, posting online, making purchases, etc.

In the following section, we will explore the use of the tools through a practical example: *What’s the weather like in Paris?*

### The weather in Paris

The tool schema used :
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
     "required": ["location"]
  }
```

<br/>

1. We will use the `TWeatherReportFunction` plugin defined in the `MistralAI.Functions.Example` unit.

```Delphi
// uses MistralAI.Functions.Example;

  var WeatherFunc: IFunctionCore := TWeatherReportFunction.Create;
```
<br/>

2. We then define a method to display the result of the query using the `Weather` tool.

```Delphi
procedure TMyForm.CalledFunction(const Value: TCalledFunction; Func: IFunctionCore);
begin
  var ArgResult := Func.Execute(Value.&Function.Arguments);

  MistralAI.Chat.AsyncCreateStream(
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
<br/>

3. Building the query using the `Weather` tool

**Synchronously code example**

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Functions.Example, MistralAI.Tutorial.FMX, MistralAI.Functions.Example;

  var WeatherFunc: IFunctionCore := TWeatherReportFunction.Create;

  TutorialHub.Tool := WeatherFunc; //For tutorial tool use only
  TutorialHub.ToolCall := CalledFunction; // For tutorial tool use only

  var Chat := MistralAI.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params.Model('mistral-large-latest');
      Params.Messages([TChatMessagePayload.User('What''s the weather like in Paris?')]);
      Params.Tools([TChatMessageTool.Add(WeatherFunc)]);
      Params.ToolChoice(auto);
      Params.MaxTokens(1024);
    end);
  try
    Display(TutorialHub, Chat);
  finally
    Chat.Free;
  end;
```

**Asynchronously code example**

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Functions.Example, MistralAI.Tutorial.FMX, MistralAI.Functions.Example;

  var WeatherFunc: IFunctionCore := TWeatherReportFunction.Create;

  TutorialHub.Tool := WeatherFunc; //For tutorial tool use only
  TutorialHub.ToolCall := CalledFunction; // For tutorial tool use only

  MistralAI.Chat.ASyncCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('mistral-large-latest');
      Params.Messages([TChatMessagePayload.User('What''s the weather like in Paris?')]);
      Params.Tools([TChatMessageTool.Add(WeatherFunc)]);
      Params.ToolChoice(auto);
      Params.MaxTokens(1024);
    end,
    function : TAsynChat
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```
<br/>

>[!NOTE]
>If we examine the `display` method in detail in this case, we notice that it takes into account the `FinishReason` flag and checks whether it is assigned the value `TFinishReason.tool_calls`. If so, the function is evaluated using the arguments obtained from the prompt.
>
>```Pascal
> procedure Display(Sender: TObject; Value: TChat);
> begin
>   for var Item in Value.Choices do
>    if Item.FinishReason = TFinishReason.tool_calls then
>      begin
>         if Assigned(TutorialHub.ToolCall) then
>           TutorialHub.ToolCall(Item.Message.ToolsCalls[0], TutorialHub.Tool);
>       end
>     else
>       begin
>         Display(Sender, Item.Message.Content);
>       end;
> end;
>```

## JSON mode

Users have the option to set response_format to {"type": "json_object"} to enable JSON mode. It's important to explicitly ask the model to generate JSON output in your message. Currently, JSON mode is available for all of the models through API.

Refer to the [official documentation](https://docs.mistral.ai/capabilities/json_mode/)

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Functions.Example, MistralAI.Tutorial.FMX;

  var Chat := MistralAI.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params.Model('mistral-tiny');
      Params.Messages([Payload.User('--- my request ---')]);
      Params.ResponseFormat(); //Enable JSON mode 
      Params.MaxTokens(1024);
    end);
  try
    Display(TutorialHub, Chat);
  finally
    Chat.Free;
  end;
```

<br/>

## Code generation

**Codestral** is an advanced generative model optimized for code generation, including **fill-in-the-middle** and code completion. Trained on over 80 programming languages, it performs well on both common and rare languages.
See also [Code generation](https://docs.mistral.ai/capabilities/code_generation/) at the MistralAI web site.

<br/>

### Before using

To utilize the Delphi classes managing the **Codestral** function, you are required to create a new KEY on the ***Mistral.ai website***. Please note that obtaining this key necessitates providing a valid phone number. 
Go to this address to create a key for using **Codestral** [Key creation](https://console.mistral.ai/codestral)

> [!TIP]
> In the above examples we use synchronous methods. Here is the asynchronous equivalent :
>
>  - `procedure AsyncCreate(ParamProc: TProc<TCodestralParams>; CallBacks: TFunc<TAsynCode>);`
>
>  - `procedure AsyncCreateStream(ParamProc: TProc<TCodestralParams>; CallBacks: TFunc<TAsynCodeStream>);`
>

<br/>

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

### Code completion

**Synchronously code example**

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.Memo2.Text := CodeBefore; // For tutorial tool use only

  var Codestral := CodingModel.Codestral.Create(
    procedure (Params: TCodestralParams)
    begin
      Params.Model('codestral-latest');
      Params.Prompt(CodeBefore);
      Params.MaxTokens(1024);
    end);
  try
    Display(TutorialHub, CodeStral);
  finally
    Codestral.Free;
  end;
```

**Asynchronously code example**

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.Memo2.Text := CodeBefore; // For tutorial tool use only

  CodingModel.Codestral.AsyncCreate(
    procedure (Params: TCodestralParams)
    begin
      Params.Model('codestral-latest');
      Params.Prompt(CodeBefore);
      Params.MaxTokens(1024);
    end,
    function : TAsynCode
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);  
```

<br/>

### Streamed Code completion

**Synchronously code example**

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.Memo2.Text := CodeBefore; // For tutorial tool use only

  CodingModel.Codestral.CreateStream(
    procedure(Params: TCodestralParams)
    begin
      Params.Model('codestral-latest');
      Params.Prompt(CodeBefore);
      Params.MaxTokens(1024);
      Params.Stream;
    end,
    procedure(var Code: TCodestral; IsDone: Boolean; var Cancel: Boolean)
    begin
      if (not IsDone) and Assigned(Code) then
        begin
          DisplayStream(TutorialHub, Code);
        end
    end);
```

**Asynchronously code example**

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.Memo2.Text := CodeBefore; // For tutorial tool use only

  CodingModel.Codestral.ASyncCreateStream(
    procedure(Params: TCodestralParams)
    begin
      Params.Model('codestral-latest');
      Params.Prompt(CodeBefore);
      Params.MaxTokens(1024);
      Params.Stream;
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

<br/>

### Fill in the middle

This feature allows users to set the beginning of their code with a `prompt` and to specify the end of the code using an optional `suffix` and an optional `stop` condition. The Codestral model will then produce the code that seamlessly fits between these markers, making it perfect for tasks that need a particular segment of code to be created.

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

The model will create the intermediate code completing the codes provided to the `prompt` and `suffix` parameters.

<br/>

### Stop tokens

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

<br/>

### End points

**Codestral** can be used directly to generate code using the endpoint: `https://codestral.mistral.ai/v1/fim/completions`, and for chat interactions with the endpoint: `https://codestral.mistral.ai/v1/chat/completions`.

However, it is crucial to understand that chat usage requires using only the **"codestral-latest"** model or similar. In other words, with the endpoint `https://codestral.mistral.ai/v1/chat/completions`, a model such as **"open-mixtral-8x22b-2404"** or similar cannot be used; instead, **"codestral-latest" should be preferred**.

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  CodingModel.Chat.ASyncCreate(
    procedure (Params: TChatParams)
    begin
      Params.Model('codestral-latest');
      Params.Messages([Payload.User('Define the term "decorum"')]);
      Params.MaxTokens(1024);
    end,
    function : TAsynChat
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

<br/>

## Files

File management APIs are especially useful for `fine-tuning` and `batch` processing operations.
- For fine-tuning, they allow uploading the data files required for training and testing a model to be fine-tuned. These dataset files can also be listed for review.
- For batch processing, files can be uploaded, retrieved, or deleted. Once the processing is complete, a result file is generated, which can then be downloaded via the APIs.

>[!NOTE]
> In the following section on file management, all code examples will be presented in asynchronous mode to enhance readability and simplify comprehension.
>

### List of files

To retrieve the list of files, proceed as follows.

1. The list can be paginated, and you can also specify the number of items to return as a parameter. Below is an example of code to display the first page:

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.Page := 0; // For tutorial tool use only

  MistralAI.&File.ASyncList(
    procedure (Params: TListParams)
    begin
      Params.PageSize(4);
    end,
    function : TAsynFiles
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

2. To navigate to the next page, use the following code:

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.NextPage; // For tutorial tool use only

  MistralAI.&File.ASyncList(
    procedure (Params: TListParams)
    begin
      Params.Page(TutorialHub.Page);
      Params.PageSize(4);
    end,
    function : TAsynFiles
    begin
      Result.Sender := TutorialHub;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

<br/>

### File Retrieve

To locate a file, you need to know its ID, which can be retrieved by displaying the list of files.

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.FileId := Id; //e.g. 982d2bbc-adbb-4c6e-b072-5aec973b4c86

  MistralAI.&File.ASyncRetrieve(TutorialHub.FileId,
    function : TAsynFile
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);  
```

<br/>

### File Upload

It is essential to populate the Purpose field to specify the category to which the file to be uploaded belongs. This can be, for instance, fine-tune or batch.

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.FileName := 'Z:\my_folder\my_file.jsonl'; // For tutorial tool use only

  MistralAI.&File.AsyncUpload(
    procedure (Params: TUploadParams)
    begin
      //Params.Purpose(TFilePurpose.finetune);
      //                              or
      //Params.Purpose(TFilePurpose.batch);
      Params.&File(TutorialHub.FileName);
    end,
    function : TAsynFile
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

<br/>

### File Delete

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.FileId := Id; //e.g. 982d2bbc-adbb-4c6e-b072-5aec973b4c86

  MistralAI.&File.ASyncDelete(TutorialHub.FileId,
    function : TAsynFilesDelete
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

<br/>

### File Download

Used to download a file generated following batch processing. To obtain the file ID, you need to view the list of available files. Alternatively, the ID can also be retrieved directly from the platform.

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.FileId := Id; //e.g. 982d2bbc-adbb-4c6e-b072-5aec973b4c86
  TutorialHub.FileOverride := True;
  TutorialHub.FileName := 'ResultFile.jsonl';

  MistralAI.&File.ASyncDownload(TutorialHub.FileId,
    function : TAsynDownLoadFile
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);  
```

<br/>

## Fine-tuning

When choosing between prompt engineering and fine-tuning for an AI model, it's advisable to start with prompt engineering due to its speed and lower resource requirements. Fine-tuning, however, offers better performance and alignment with specific tasks, making it ideal for specialized applications and cost reduction.

See also [Fine-tuning description](https://docs.mistral.ai/capabilities/finetuning/) at the **MistralAI** web site.

> [!TIP]
> Synchronous and asynchronous methods also exist for fine-tuning as well as file handling. See the TFilesRoute class in the MistralAI.Files unit and the TFineTuningRoute class in the MistralAI.FineTunings unit

<br/>

Data should be stored in **JSON** Lines files `(.jsonl)`, where each line corresponds to a separate **JSON object**. This format enables efficient storage of multiple **JSON objects**.

The datasets must adhere to an instruction-following format that simulates a conversation between a user and an assistant. Each JSON record should either contain only messages exchanged between the user and the assistant (referred to as ***"Default Instruct"***), or include additional logic for function calls (referred to as ***"Function-calling Instruct"***). 

See also [Default Instruct](https://docs.mistral.ai/capabilities/finetuning/#1-default-instruct) and [Function-calling Instruct](https://docs.mistral.ai/capabilities/finetuning/#2-function-calling-instruct)

**`Warning:`**
Please remember to remove any line breaks if you copy and paste the examples provided by Mistral AI for the "Dataset Format."

### Create a Fine-tuning Job

The next step involves creating a fine-tuning job.

- **model**: Select the specific model you wish to fine-tune, with options being "open-mistral-7b" and "mistral-small-latest".
- **training_files**: Provide a set of training file IDs, which can include one or multiple files.
- **validation_files**: Provide a set of validation file IDs, which can include one or multiple files.
- **hyperparameters**: Adjust the two available hyperparameters, "trainingₛtep" and "learning_rate", according to your preferences.


The List/Retrieve/Cancel methods are also available to manage Jobs. And for the last two functions you will have to provide the job ID as a parameter.

**Important Notice**

As of July 2024, the only fine-tunable models at Mistral are                 
  - `open-mistral-7b (v0.3)`,                                               
  - `mistral-small-latest` (`mistral-small-2402`),                            
  - `codestral-latest` (`codestral-2405`),                                    
  - `open-mistral-nemo` and ,                                               
  - `mistral-large-latest` (`mistral-large-2407`)

**Example**

**Synchronously code example**

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  var FineTuneJob := MistralAI.FineTuning.CreateJob(
    procedure (Params: TFineTuningJobParams)
    begin
      Params.Model('open-mistral-7b'); //Fine Tuneable Models : Enum "open-mistral-7b", "mistral-small-latest
      Params.TrainingFiles([Id_TrainingFile1, Id_TrainingFile2, ... ]);
      Params.ValidationFiles([Id_ValidationFile1, Id_ValidationFile2, ... ]);
      Params.Suffix('ft-model-01');  //less than 18 characters
      Params.Hyperparameters(
        procedure (var Params: THyperparametersParams)
        begin
          Params.TrainingSteps(10);
          Params.LearningRate(0.0005);
        end);
    end
  );
  try
    Display(TutorialHub, FineTuneJob);  
  finally
    FineTuneJob.Free;
  end;
```

<br/>

### Delete a Fine-tuned Model

`Note`: The method in charge of deleting a fine tuned model is found in the `MistralAI.Models.pas` unit.

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  with MistralAI.Models.Delete('Id_Model_to_delete') do
  try
    ShowMessage('Model Deleted');
  finally
    Free;
  end;
```

<br/>

### List of Fine-tune Job

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  MistralAI.FineTuning.ASyncList(
    procedure (Params: TFineTuningJobListParams)
    begin
      Params.Page(100);
    end,
    function : TAsynListFineTuningJobs
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

>[!NOTE]
> You can use the pagination method described in the [`File List` section](#List-of-files).
>

<br/>

### Retrieve a Fine-tune Job

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.JobId := Id; //e.g. 982d2bbc-adbb-4c6e-b072-5aec973b4c86

  MistralAI.FineTuning.ASyncRetrieve(TutorialHub.JobId,
    function : TAsynJobOutProgress
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

<br/>

### Start a Fine-tune Job

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.JobId := Id; //e.g. 982d2bbc-adbb-4c6e-b072-5aec973b4c86

  MistralAI.FineTuning.ASyncStart(TutorialHub.JobId,
    function : TAsynJobOutProgress
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

<br/>

### Cancel a Fine-tune Job

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.JobId := Id; //e.g. 982d2bbc-adbb-4c6e-b072-5aec973b4c86

  MistralAI.FineTuning.ASyncCancel(TutorialHub.JobId,
    function : TAsynJobOutProgress
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

>[!WARNING]
> The APIs do not provide the ability to delete a fine-tuning job, and this limitation also applies to the platform. Consequently, creating a large number of jobs could become problematic over time.
>

<br/>

## Moderation

The moderation service, leveraging the advanced Mistral Moderation model—a classifier built on the Ministral 8B 24.10 framework—empowers users to identify harmful text content across multiple policy dimensions.

The service offers two distinct endpoints: one designed for the classification of raw text and another specifically tailored for conversational content. Further details are provided below.
- **Raw-text endpoint:** `https://api.mistral.ai/v1/moderations`
- **Conversational endpoint:** `https://api.mistral.ai/v1/chat/moderations`

>[!NOTE]
> This note is sourced from the official MistralAI documentation.
>
> The policy threshold is determined based on the optimal performance of our internal test set. You can use the raw score or adjust the threshold according to your specific use cases.
>
> We intend to continually improve the underlying model of the moderation endpoint. Custom policies that depend on category_scores can require recalibration.
>

Refer to the [official documentation](https://docs.mistral.ai/capabilities/guardrailing/) about moderation.

<br/>

### Raw-text endpoint

In this example, only the categories requiring moderation consideration are displayed.

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  var Str := '...text to classify...';
  Display(TutorialHub, Str);

  MistralAI.Classifiers.ASyncModeration(
    procedure (Params: TModerationParams)
    begin
      Params.Model('mistral-moderation-latest');
      Params.Input(Str);
    end,
    function : TAsynModeration
    begin
      Result.Sender := TutorialHub;
      Result.OnSuccess := DisplayEx;
      Result.OnError := Display;
    end);
```

<br/>

### Conversational endpoint

In this example, all moderation categories are displayed.

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  var Str := '...text to classify...';
  Display(TutorialHub, Str);

  MistralAI.Classifiers.ASyncModerationChat(
    procedure (Params: TModerationChatParams)
    begin
      Params.Model('mistral-moderation-latest');
      Params.Input([Payload.User(Str)]);
    end,
    function : TAsynModeration
    begin
      Result.Sender := TutorialHub;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

<br/>

## Batch Inference

The batch processing feature allows sending a large number of requests simultaneously to various endpoints (chat, embeddings, moderations, etc.). Each batch contains uniquely identified requests and is processed asynchronously. This enables tracking the progress of the processing, accessing results once completed, and handling potential timeouts.

Requests can be customized using metadata, and it is possible to view or cancel ongoing processes. Final results are available as downloadable files and remain accessible afterward.

This approach provides more efficient management of high volumes of requests without imposing a strict limit on the number of batches, while adhering to an overall limit on pending requests. It is particularly useful for automating and consolidating processing, especially in cases of large-scale or repetitive requests.

Refer to the [official documentation](https://docs.mistral.ai/capabilities/batch/).

<br/>

### Batch List

The list of batch processes can be retrieved using the following API. The pagination mechanism described for the [file list](#List-of-files) can also be used.

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  MistralAI.Batch.ASyncList(
    procedure (Params: TBatchJobListParams)
    begin
      Params.PageSize(100);
    end,
    function : TAsynBatchJobList
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);

// Synchronously code
//  var List := MistralAI.Batch.List(
//    procedure (Params: TBatchJobListParams)
//    begin
//      Params.PageSize(100);
//    end);
//  try
//    Display(TutorialHub, List);
//  finally
//    List.Free;
//  end;
```

<br/>

### Batch Job Create

We take the batch given in the example on the official site

Here's an example of how to structure a batch request:

```Json
{"custom_id": "0", "body": {"max_tokens": 100, "messages": [{"role": "user", "content": "What is the best French cheese?"}]}}
{"custom_id": "1", "body": {"max_tokens": 100, "messages": [{"role": "user", "content": "What is the best French wine?"}]}}
```

<br/>

1. Save this text in a file and name it as you prefer.
2. Next, upload the file using the code provided in the [File Upload section](#File-Upload).
3. Be sure to set the value of Purpose to batch.
4. Note the file ID after it has been uploaded, as this `ID` will be required when creating the batch processing job.

<br/>

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  MistralAI.Batch.ASyncCreateJob(
    procedure (Params: TBatchJobParams)
    begin
      Params.InputFiles([ID]);
      Params.Model('mistral-large-latest');
      Params.Endpoint(TEndPointType.epChatCompletion);
      Params.Metadata(TJSONObject.Create.AddPair('job_type', 'texting'));
    end,
    function : TAsynBatchJob
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);

// Synchronously code
//  var Value := MistralAI.Batch.CreateJob(
//    procedure (Params: TBatchJobParams)
//    begin
//      Params.InputFiles([ID]);
//      Params.Model('mistral-large-latest');
//      Params.Endpoint(TEndPointType.epChatCompletion);
//      Params.Metadata(TJSONObject.Create.AddPair('job_type', 'texting'));
//    end);
```

>[!NOTE]
> Note: The following metadata was specified during creation as a label: 
>```Json
> {"job_type": "texting"}
>```

<br/>

### Batch Job Cancel

A job can be interrupted as long as it is still being processed. To perform this action, ensure that you have obtained the job ID beforehand, then use the following code:

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.BatchId := ID; //e.g. 982d2bbc-adbb-4c6e-b072-5aec973b4c86

  MistralAI.Batch.ASyncCancel(TutorialHub.BatchId,
    function : TAsynBatchJob
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);

// Synchronously code
//  var Value := MistralAI.Batch.Cancel(TutorialHub.BatchId);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br/>

### Batch Job Retrieve

A batch can be retrieved using its ID, allowing you to check its status to track progress or obtain the ID of the file generated at the end of the process containing the expected results. To do so, use the following code:

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  TutorialHub.BatchId := ID; //e.g. 982d2bbc-adbb-4c6e-b072-5aec973b4c86

  MistralAI.Batch.ASyncRetrieve(TutorialHub.BatchId,
    function : TAsynBatchJob
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);

// Synchronously code
//  var Value := MistralAI.Batch.Retrieve(TutorialHub.BatchId);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br/>

### Batch Job Result File

If the batch processing completes successfully, an ID pointing to a file containing the results is provided. This ID can be accessed through the OutputFile field of the TBatchJob class.
In this case, you can use the [File Download API](#File-Download), demonstrated in this code example, to retrieve the file.
Alternatively, you can download the file directly from the platform.

<br/>

>[!WARNING]
> The APIs do not support the deletion of batch jobs, and this limitation extends to the platform itself. As a result, creating a large number of batch jobs over time may lead to management challenges.
>

<br/>

## Agents

The official documentation provided by Mistral regarding agents is available [here](https://docs.mistral.ai/capabilities/agents/).

> [!TIP]
> The execution of an agent can be done both synchronously and asynchronously. See the class `TAgentRoute` in the **MistralAI.Agents** unit.

<br/>

Before using this asynchronous code and after adapting it to your specific needs, you must create an agent on the platform and retrieve its `ID`.

```Pascal
// MistralAI, MistralAI.Types, MistralAI.Tutorial.FMX;

  MistralAI.Agent.AsyncCreateStream(
    procedure (Params: TAgentParams)
    begin
      Params.MaxTokens(1024);
      Params.Messages([
        Payload.User('Content_1')
        ]);
      Params.AgentId('Agent_Id');
      Params.Stream;
    end,
    function : TAsynChatStream
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnProgress := DisplayStream;
      Result.OnError := Display;
    end);
```

<br/>

> [!WARNING]
> As of 08/13/2024, only the API for executing an agent is available; however, no API for creating an agent has been made available.  
>
> (See the [***MistralAI.Agents.pas***](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/source/MistralAI.Agents.pas) unit)
>
> To create an agent you must go through the [platform](https://console.mistral.ai/build/agents/new)

<br/>

# Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

<br/>

# License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.

