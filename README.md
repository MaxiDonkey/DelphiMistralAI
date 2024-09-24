# Delphi MistralAI API

___
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.3/11/12-yellow)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-green)
![GitHub](https://img.shields.io/badge/Updated%20the%2009/23/2024-blue)
<br/>
<br/>
<br/>
- [Introduction](#Introduction)
- [Remarks](#remarks)
- [Usage](#usage)
    - [Initialization](#initialization)
    - [Models](#models)
    - [Embeddings](#embeddings)
    - [Chats](#chats)
        - [Synchronous](#Synchronous)
        - [Asynchronous](#Asynchronous)
    - [Vision](#vision)
        - [Passing an Image URL](#Passing-an-image-url)
        - [Passing a Base64 Encoded Image](#Passing-a-base64-encoded-image)	
    - [Function calling](#function-calling)
    - [JSON mode](#JSON-mode)
    - [Code generation](#Code-generation)
        - [Before using](#Before-using)
        - [Codestral initialization](#Codestral-initialization)
        - [Code completion](#Code-completion)
        - [Streamed Code completion](#Streamed-code-completion)
        - [Fill in the middle](#Fill-in-the-middle)
        - [Stop tokens](#Stop-tokens)
        - [End points](#End-points)
    - [Fine-tuning](#Fine-tuning)
        - [Files](#Files)
        - [Create a fine-tuning job](#Create-a-fine-tuning-job)
        - [Delete a fine-tuned model](#Delete-a-fine-tuned-model)
    - [Agents](#Agents)
- [Contributing](#contributing)
- [License](#license)
<br/>
<br/>
<br/>
<br/>

## Introduction

Welcome to the unofficial Delphi **MistralAI** API library. This project aims to provide a `Delphi` interface for interacting with the **MistralAI** public API, making it easier to integrate advanced natural language processing features into your `Delphi` applications. Whether you want to generate text, create embeddings, use chat models, or generate code, this library offers a simple and effective solution.

**MistralAI** is a powerful natural language processing API that enables developers to incorporate advanced AI functionalities into their applications. For more details, visit the [official MistralAI documentation](https://docs.mistral.ai/api/).

<br/>
<br/>

## Remarks 

> [!IMPORTANT]
>
> This is an unofficial library. **MistralAI** does not provide any official library for `Delphi`.
> This repository contains `Delphi` implementation over [MistralAI](https://docs.mistral.ai/api/) public API.

<br/>
<br/>

## Usage

### Initialization

To initialize the API instance, you need to [obtain an API token from MistralAI](https://console.mistral.ai/api-keys/).

Once you have a token, you can initialize `IMistralAI` interface, which is an entry point to the API.

Due to the fact that there can be many parameters and not all of them are required, they are configured using an anonymous function.

```Pascal
uses MistralAI;

var MistralAI: IMistralAI := TMistralAI.Create(API_TOKEN);
```

You can also go through the factory:

> [!NOTE]
>```Pascal
>uses MistralAI;
>
>var MistralAI := TMistralAIFactory.CreateInstance(API_TOKEN);
>```

<br/>
<br/>

### Models

List the various models available in the API. You can refer to the Models documentation to understand what models are available.
See [Models Documentation](https://docs.mistral.ai/models/)

```Pascal
//uses MistralAI, MistralAI.Models;

var Models := MistralAI.Models.List; 
  try
    for var Model in Models.Data do
      if Model.Capabilities.FineTuning then
        Memo1.Lines.Add(Model.id + '  (Can be fine-tuned)') else
        Memo1.Lines.Add(Model.id)
  finally
    Models.Free;
  end;
```

Asynchronously, we proceed as follows:

> [!TIP]
>```Pascal
>//uses MistralAI, MistralAI.Models;
>
>MistralAI.Models.AsyncList(
>    function : TAsyncModelsParams
>    begin
>      Result.Sender := Memo1;  //Uses a TMemo for displaying
>
>      Result.OnStart := nil;
>        
>      Result.OnSuccess :=
>        procedure (Sender: TObject; List: TModels)
>        begin
>          var M := Sender as TMemo;
>
>          M.Lines.BeginUpdate;
>          try
>            for var Model in List.Data do
>              if Model.Capabilities.FineTuning then
>                M.Lines.Add(Model.id + '  (Can fine-tuned)') else
>                M.Lines.Add(Model.id)
>              end
>          finally
>            M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
>            M.Lines.EndUpdate;	
>          end;
>    end);
>```

The `TModelRoute` class exposes the following methods: 
  - `function Delete(const ModelId: string): TModelDeletion` 
  - `function Retrieve(const ModelId: string): TModel` 
  - `function Update(const ModelId: string; ParamProc: TProc< TModelParams>): TFineTunedModel` 
  - `function Archive(const ModelId: string): TArchivingModel` 
  - `function Unarchive(const ModelId: string): TArchivingModel` 

<br/>

> [!TIP]
>As well as their asynchronous equivalent 
>  - `procedure AsyncDelete(const ModelId: string; const CallBacks: TFunc<TAsyncModelDeletion>);` 
>  - `procedure AsyncRetrieve(const ModelId: string; const CallBacks: TFunc<TAsyncModelParams>)` 
>  - `procedure AsyncUpdate(const ModelId: string; ParamProc: TProc<TModelParams>; const CallBacks: TFunc<TAsyncFineTuneModelParams>)` 
>  - `procedure AsyncArchive(const ModelId: string; const CallBacks: TFunc<TAsynArchivingModelParams>)` 
>  - `procedure AsyncUnarchive(const ModelId: string; const CallBacks: TFunc<TAsynArchivingModelParams>)`

<br/>
<br/>

### Embeddings

Embeddings make it possible to vectorize one or more texts in order, for example, to calculate the similarity between sentences. Each vector resulted will be of dimension 1024. This vector representation captures deep semantic aspects of texts, allowing for more nuanced comparisons.
Distance measures such as cosine, Euclidean distance or other custom measures can be applied to these embeddings. 

See also [tokenization](https://docs.mistral.ai/guides/tokenization/) at the MistralAI web site.

```Pascal
//uses MistralAI, MistralAI.Embeddings;

  var Embeddings := MistralAI.Embeddings.Create(
    procedure (Params: TEmbeddingParams)
    begin
      Params.Model('mistral-embed'); //By default this is the model used so this line can be omitted
      Params.Input(['aba', 'bbb']);
    end);
  try
    for var Value in Embeddings.Data do
      begin
        Memo1.Lines.Add('-----------------------------' + Value.index.ToString);
        for var Item in Value.Embedding do
          Memo1.Lines.Add(Item.ToString);
      end;
  finally
    Embeddings.Free;
  end;
```

<br/>
<br/>

Asynchronously, we proceed as follows:

> [!TIP]
>```Pascal
>//uses MistralAI, MistralAI.Embeddings;
>
>  MistralAI.Embeddings.AsyncCreate(
>    procedure (Params: TEmbeddingParams)
>    begin
>      Params.Input(['Text to vectorize']);
>    end,
>
>    function : TAsyncEmbeddingsParams
>    begin
>      Result.Sender := Memo1;
>
>      Result.OnSuccess :=
>        procedure (Sender: TObject; Result: TEmbeddings)
>        begin
>          var M := Sender as TMemo;
>          M.Lines.BeginUpdate;
>          try
>            for var Value in Result.Data do
>              begin
>                M.Lines.Add('-----------------------------' + Value.index.ToString);
>                for var Item in Value.Embedding do
>                  M.Lines.Add(Item.ToString);
>              end;
>          finally
>            M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
>            M.Lines.EndUpdate;
>          end;
>        end
>    end);
>```

<br/>
<br/>

### Chats

Using the API to create and maintain conversations. You have the option to either wait for a complete response or receive the response sequentially (Streaming mode).

See also [Prompting Capabilities](https://docs.mistral.ai/guides/prompting_capabilities/) at the MistralAI web site.

<br/>

#### Synchronous

```Pascal
//uses MistralAI, MistralAI.Functions.Tools, MistralAI.Chat;  

  var Chat := MistralAI.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params.Model('mistral-tiny');
      Params.Messages([TChatMessagePayload.User(Memo2.Text)]);
      Params.MaxTokens(1024);
    end);
  try
    for var Choice in Chat.Choices do
      Memo1.Lines.Add(Choice.Message.Content);
  finally
    Chat.Free;
  end;
```

<br/>
<br/>

### Stream mode

```Pascal
//uses MistralAI, MistralAI.Functions.Tools, MistralAI.Chat;

  MistralAI.Chat.CreateStream(
    procedure(Params: TChatParams)
    begin
      Params.Model('mistral-medium');
      Params.Messages([TChatMessagePayload.User(Memo2.Text)]);
      Params.MaxTokens(1024);
      Params.Stream;
    end,
    procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
    begin
      if (not IsDone) and Assigned(Chat) then
        begin
          Memo1.Text := Memo1.Text + Chat.Choices[0].Delta.Content;
          Application.ProcessMessages;
        end
      else if IsDone then 
        Memo1.Text := Memo1.Text + '--- Done';
      Sleep(30);
    end);
```

<br/>
<br/>

#### Asynchronous

You can use asynchronous methods for text completion or chat tasks. For this, you need to use the two methods

 1. `procedure AsyncCreate(ParamProc: TProc<TChatParams>; Events: TFunc<TAsynChatParams>)`

 2. `procedure AsyncCreateStream(ParamProc: TProc<TChatParams>; Events: TFunc<TAsynChatStreamParams>)`

as follows :

 1 . Asynchronous mode : See `TAsynChatParams = record`

> [!TIP]
>```Pascal
>//uses MistralAI, MistralAI.Functions.Tools, MistralAI.Chat;
>
>  MistralAI.Chat.AsyncCreate(
>      procedure (Params: TChatParams)
>      begin
>        Params.Model('my_model');
>        Params.Messages([TChatMessagePayload.User('Hello')]);
>        Params.MaxTokens(1024);
>      end,
>
>      function : TAsynChatParams
>      begin
>        Result.Sender := Memo1; //Uses TMemo for displaying 
>
>        Result.OnStart := nil;
>
>        Result.OnSuccess :=
>          procedure (Sender: TObject; Chat: TChat)
>          begin
>            var M := Sender as TMemo;
>            for var Choice in Chat.Choices do
>              M.Lines.Add(Choice.Message.Content + sLineBreak);
>          end;
>
>        Result.OnError :=
>          procedure (Sender: TObject; value: string)
>          begin
>            ShowMessage(Value);
>          end;
>      end);
>```
>

<br/>
<br/>

 2. Asynchronous stream mode : See `TAsynChatStreamParams = record`

> [!TIP]
>```Pascal
>//uses MistralAI, MistralAI.Functions.Tools, MistralAI.Chat;
>
>  MistralAI.Chat.AsyncCreateStream(
>       procedure(Params: TChatParams)
>       begin
>         Params.Model('my_model');
>         Params.Messages([TChatMessagePayload.User('request')]);
>         Params.MaxTokens(1024);
>         Params.Stream;
>       end,
>    
>       function: TAsynChatStreamParams
>       begin
>         Result.Sender := Memo1;  //Events will return this instance
>    
>         Result.OnStart := nil;   // if nil then; Can be omitted
>
>         Result.OnProgress :=
>           procedure (Sender: TObject; Chat: TChat)
>           begin
>             // Handle progressive updates to the chat response
>             var S := Chat.Choices[0].Delta.Content;
>             var M := Sender as TMemo;
>             M.Lines.BeginUpdate;
>             try
>               for var i := 1 to S.Length do
>                 if (S[i] <> #10) and (S[i] <> #13) then
>                   M.Text := M.Text + S[i] else
>                   M.Text := M.Text + sLineBreak;
>               M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
>             finally
>               M.Lines.EndUpdate;
>             end;		
>           end;
>    
>         Result.OnSuccess :=
>           procedure (Sender: TObject)
>           begin
>             // Handle success when the operation completes
>           end;
>    
>         Result.OnError :=
>           procedure (Sender: TObject; Value: string)
>           begin
>             ShowMessage(Value); // Display error message
>           end;
>    
>         Result.OnDoCancel :=
>           function: Boolean
>           begin
>             Result := CheckBox1.Checked; // Click on checkbox to cancel
>           end;
>
>         Result.OnCancellation :=
>           procedure (Sender: TObject)
>           begin
>             // Processing when process has been canceled
>           end;
>       end);
>```
>

<br/>
<br/>

### Vision

The latest Pixtral 12B adds vision capabilities, allowing to analyze both images and text, expanding its potential for applications requiring multimodal understanding.
See also [official documentation](https://docs.mistral.ai/capabilities/vision/).

To support both synchronous and asynchronous completion methods, we focused on generating the appropriate payload for message parameters. An overloaded version of the `TChatMessagePayload.User` class function was added, allowing users to include a dynamic array of text elements—either URLs or file paths—alongside the user's text content. Internally, this data is processed to ensure the correct operation of the vision system in both synchronous and asynchronous contexts.

<br/>

#### Passing an Image URL

```Pascal
//uses MistralAI, MistralAI.Functions.Tools, MistralAI.Chat;

  var Chat := MistralAI.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params.Model('pixtral-12b-2409');
      
      // png, jpeg, gif and webp formats are supported
      var Ref1 := 'https://tripfixers.com/wp-content/uploads/2019/11/eiffel-tower-with-snow.jpeg';
      var Ref2 := 'https://assets.visitorscoverage.com/production/wp-content/uploads/2024/04/AdobeStock_626542468-min-1024x683.jpeg';
      
      Params.Messages([TChatMessagePayload.User('what are the differences between two images?', [Ref1, Ref2])]);

      Params.MaxTokens(1024);
    end);
  try
    for var Choice in Chat.Choices do
      Memo1.Lines.Add(Choice.Message.Content);
  finally
    Chat.Free;
  end;
  
```

<br/>
<br/>

#### Passing a Base64 Encoded Image


```Pascal
//uses MistralAI, MistralAI.Functions.Tools, MistralAI.Chat;

  var Chat := MistralAI.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params.Model('pixtral-12b-2409');
      
      var Ref := 'D:\My_folder\Images\my_image.png';  // png, jpeg, gif and webp formats are supported
          
      Params.Messages([TChatMessagePayload.User('my query ', [Ref])]);

      Params.MaxTokens(1024);
    end);
  try
    for var Choice in Chat.Choices do
      Memo1.Lines.Add(Choice.Message.Content);
  finally
    Chat.Free;
  end;
  
```

<br/>
<br/>

### Function calling

Function calling allows Mistral models to connect to external tools. By integrating Mistral models with external tools such as user defined functions or APIs, users can easily build applications catering to specific use cases and practical problems. 

See also [documentation](https://docs.mistral.ai/capabilities/function_calling/) at the MistralAI web site.

Warning : While this technology is powerful, it also carries potential risks. We strongly advise incorporating user confirmation processes before executing real-world actions on behalf of users, such as sending emails, posting online, making purchases, etc.


```Pascal
//uses 
//  MistralAI, MistralAI.Chat,  
//  MistralAI.Functions.Core, MistralAI.Functions.Tools, MistralAI.Functions.Example;
  
  var WeatherFunc: IFunctionCore := TWeatherReportFunction.Create; //plugin in charge of the external API that can be invoked by the model  
  var Chat := MistralAI.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params.Model('mistral-small-latest');
      Params.Messages([TChatMessagePayload.User(Memo2.Text)]);
      Params.SafePrompt(False);
      Params.Stream(False);
      Params.Temperature(0.7);
      Params.TopP(1);
      Params.Tools([TChatMessageTool.Add(WeatherFunc)]);
      Params.ToolChoice(auto);
      Params.MaxTokens(64);
      Params.RandomSeed(1337);
    end);
  try
    for var Choice in Chat.Choices do
      begin
        if Choice.FinishReason = TFinishReason.tool_calls then
          CallFunction(Choice.Message.ToolsCalls[0], WeatherFunc)
        else
          Memo1.Lines.Add(Choice.Message.Content); //Display message content if function is not called
      end;
  finally
    Chat.Free;
  end;

procedure CallFunction(const Value: TCalledFunction; Func: IFunctionCore);
begin
  var ArgResult := Func.Execute(Value.&Function.Arguments);
  var Chat := MistralAI.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params.Model('open-mixtral-8x22b-2404');
      Params.Messages([
        TChatMessagePayload.User(Memo2.Text),
        TChatMessagePayload.User(ArgResult)
      ]);
      Params.MaxTokens(1024);
    end);
  try
    for var Choice in Chat.Choices do
      Memo1.Lines.Add(Choice.Message.Content); //Display message content
  finally
    Chat.Free;
  end;
end;
```

<br/>
<br/>

### JSON mode

Users have the option to set response_format to {"type": "json_object"} to enable JSON mode. It's important to explicitly ask the model to generate JSON output in your message. Currently, JSON mode is available for all of the models through API.

See also [documentation](https://docs.mistral.ai/capabilities/json_mode/) at the MistralAI web site.

```Pascal
//uses MistralAI, MistralAI.Chat;  

  var Chat := MistralAI.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params.Model('mistral-tiny');
      Params.Messages([TChatMessagePayload.User(Memo2.Text)]);
      Params.ResponseFormat(); //Enable JSON mode 
      Params.MaxTokens(1024);
    end);
  try
    for var Choice in Chat.Choices do
      Memo1.Lines.Add(Choice.Message.Content);
  finally
    Chat.Free;
  end;
```

<br/>
<br/>

### Code generation

**Codestral** is an advanced generative model optimized for code generation, including **fill-in-the-middle** and code completion. Trained on over 80 programming languages, it performs well on both common and rare languages.
See also [Code generation](https://docs.mistral.ai/capabilities/code_generation/) at the MistralAI web site.

<br/>
<br/>

#### Before using

To utilize the Delphi classes managing the **Codestral** function, you are required to create a new KEY on the ***Mistral.ai website***. Please note that obtaining this key necessitates providing a valid phone number. 
Go to this address to create a key for using **Codestral** [Key creation](https://console.mistral.ai/codestral)

> [!TIP]
> In the above examples we use synchronous methods. Here is the asynchronous equivalent :
>
>  - `procedure AsyncCreate(ParamProc: TProc<TCodestralParams>; CallBacks: TFunc<TAsynCodeParams>);`
>
>  - `procedure AsyncCreateStream(ParamProc: TProc<TCodestralParams>; CallBacks: TFunc<TAsynCodeStreamParams>);`
>

<br/>
<br/>

#### Codestral initialization

When instantiating the interface managing the ***TMistralAI*** type class, the `CodestralSpec` specification must be specified in the `create` constructor.

The resulting interface will handle both **CodeStral** functionality as well as chat-type interactions.

```Pascal
uses MistralAI;

var CodingModel: IMistralAI := TMistralAI.Create(API_TOKEN, [CodestralSpec]);
```

You can also go through the factory:
> [!NOTE]
> ```Pascal
> uses MistralAI;
>
> var CodingModel := TMistralAIFactory.CreateInstance(API_TOKEN, [CodestralSpec]);
> ```

<br/>
<br/>

#### Code completion

```Pascal
//uses MistralAI, MistralAI.Codestral;

  var Codestral := CodingModel.Codestral.Create(
    procedure (Params: TCodestralParams)
    begin
      Params.Model('codestral-latest');
      Params.Prompt(Memo2.Text);  
      Params.MaxTokens(1024);
    end);
  try
    for var Choice in Codestral.Choices do
      Memo1.Lines.Add(Choice.Message.Content);
  finally
    Codestral.Free;
  end;
```

<br/>
<br/>

#### Streamed Code completion

```Pascal
//uses MistralAI, MistralAI.Codestral;

  CodingModel.Codestral.CreateStream(
    procedure(Params: TCodestralParams)
    begin
      Params.Model('codestral-latest');
      Params.Prompt(Memo2.Text);
      Params.MaxTokens(1024);
      Params.Stream;
    end,
    procedure(var Code: TCodestral; IsDone: Boolean; var Cancel: Boolean)
    begin
      if (not IsDone) and Assigned(Code) then
        begin
          Memo1.Text := Memo1.Text + Code.Choices[0].Delta.Content;
          Application.ProcessMessages;
        end
      else if IsDone then ;
      Sleep(30);
    end);
```

<br/>
<br/>

#### Fill in the middle

This feature allows users to set the beginning of their code with a `prompt` and to specify the end of the code using an optional `suffix` and an optional `stop` condition. The Codestral model will then produce the code that seamlessly fits between these markers, making it perfect for tasks that need a particular segment of code to be created.

```Pascal
//uses MistralAI, MistralAI.Codestral;

  CodingModel.Codestral.CreateStream(
    procedure(Params: TCodestralParams)
    begin
      Params.Model('codestral-latest');

      Params.Prompt(Memo2.Text); // Beginning text
      Params.Suffix(Memo3.Text); // Text ending

      Params.MaxTokens(1024);
      Params.Stream;
    end,
    procedure(var Code: TCodestral; IsDone: Boolean; var Cancel: Boolean)
    begin
      if (not IsDone) and Assigned(Code) then
        begin
          Memo1.Text := Memo1.Text + Code.Choices[0].Delta.Content;
          Application.ProcessMessages;
        end
      else if IsDone then ;
      Sleep(30);
    end);
```

The model will create the intermediate code completing the codes provided to the `prompt` and `suffix` parameters.

<br/>
<br/>

#### Stop tokens

It is advisable to include stop tokens when integrating with IDE autocomplete to ensure the model doesn't provide overly verbose output.

```Pascal
//uses MistralAI, MistralAI.Codestral;

  var Codestral := CodingModel.Codestral.Create(
    procedure (Params: TCodestralParams)
    begin
      Params.Model('codestral-latest');
      Params.Prompt(Memo2.Text);
      Params.Suffix(Memo3.Text);
      Params.MaxTokens(1024);
      Params.Stop(['\n\n']);
    end);
  try
    for var Choice in Codestral.Choices do
      Memo1.Lines.Add(Choice.Message.Content);
  finally
    Codestral.Free;
  end;
```

<br/>
<br/>

#### End points

**Codestral** can be used directly to generate code using the endpoint: `https://codestral.mistral.ai/v1/fim/completions`, and for chat interactions with the endpoint: `https://codestral.mistral.ai/v1/chat/completions`.

However, it is crucial to understand that chat usage requires using only the **"codestral-latest"** model or similar. In other words, with the endpoint `https://codestral.mistral.ai/v1/chat/completions`, a model such as **"open-mixtral-8x22b-2404"** or similar cannot be used; instead, **"codestral-latest" should be preferred**.

```Pascal
//uses MistralAI, MistralAI.Codestral;

  var Chat := CodingModel.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params.Model('codestral-latest');
      Params.Messages([TChatMessagePayload.User(Memo2.Text)]);
      Params.MaxTokens(1024);
    end);
  try
    for var Choice in Chat.Choices do
      Memo1.Lines.Add(Choice.Message.Content);
  finally
    Chat.Free;
  end;
```

<br/>
<br/>

### Fine-tuning

When choosing between prompt engineering and fine-tuning for an AI model, it's advisable to start with prompt engineering due to its speed and lower resource requirements. Fine-tuning, however, offers better performance and alignment with specific tasks, making it ideal for specialized applications and cost reduction.

See also [Fine-tuning description](https://docs.mistral.ai/capabilities/finetuning/) at the **MistralAI** web site.

> [!TIP]
> Synchronous and asynchronous methods also exist for fine-tuning as well as file handling. See the TFilesRoute class in the MistralAI.Files unit and the TFineTuningRoute class in the MistralAI.FineTunings unit

<br/>
<br/>

#### Files

Data should be stored in **JSON** Lines files `(.jsonl)`, where each line corresponds to a separate **JSON object**. This format enables efficient storage of multiple **JSON objects**.

The datasets must adhere to an instruction-following format that simulates a conversation between a user and an assistant. Each JSON record should either contain only messages exchanged between the user and the assistant (referred to as ***"Default Instruct"***), or include additional logic for function calls (referred to as ***"Function-calling Instruct"***). 

See also [Default Instruct](https://docs.mistral.ai/capabilities/finetuning/#1-default-instruct) and [Function-calling Instruct](https://docs.mistral.ai/capabilities/finetuning/#2-function-calling-instruct)

**`Warning:`**
Please remember to remove any line breaks if you copy and paste the examples provided by Mistral AI for the "Dataset Format."

**File Upload example**

```Pascal
//uses MistralAI.Files;

  with MistralAI.&File.Upload(
    procedure (Params: TUploadParams)
    begin
      Params.&File('training_file.jsonl'); //File name to upload
      Params.Purpose(finetune);
    end)
  do
    try
      ShowMessage(Id); //Display the uploaded file Id
    finally
      Free;
    end;  
```

**Returns uploaded files list**

```Pascal
//uses MistralAI.Files;

  with MistralAI.&File.List do
  try
    for var Item in Data do
      ShowMessage(Item.Id);
  finally
    Free;
  end;
```

**Delete an uploaded file by his Id**

```Pascal
//uses MistralAI.Files;

  with MistralAI.&File.Delete(Id) do //Id : Id of the file to delete
  try
    if Deleted then
      ShowMessageFmt('File with Id=%s was deleted', [MyId]); 
  finally
    Free;
  end;
```
Returns 404 error if the Id does not exists.

**Returns information about a specific file**

```Pascal
//uses MistralAI.Files;

  var MyId := 'XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX'; //Replace by the Id of file to retrieve
  with MistralAI.&File.Retrieve(MyId) do
  try
    ShowMessageFmt('%s'#13'%s'#13'%d bytes', [Id, FileName, Bytes]);
  finally
    Free;
  end;
```
Refer to the `TFile` class in the MistralAI.Files.pas unit to understand the information returned by the "Retrieve" method.

<br/>
<br/>

#### Create a fine-tuning job

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

```Pascal
//uses MistralAI.Files, MistralAI.FineTunings;

  var MyJob := MistralAI.FineTuning.CreateAndRun(
    procedure (Params: TFineTuningJobParams)
    begin
      Params.Model('open-mistral-7b'); //Fine Tuneable Models : Enum "open-mistral-7b", "mistral-small-latest
      Params.TrainingFiles([Id_TrainingFile1, Id_TrainingFile2, ... ]);
      Params.ValidationFiles([Id_ValidationFile1, Id_ValidationFile2, ... ]);
      Params.Suffix('my-great-model');  //less than 18 characters
    end
  );
  with MyJob do
  try
    ShowMessageFmt('%s'#13'%s'#13'%s', [Id, Model, FineTuningModel]);
  finally
    Free;
  end;
```

<br/>
<br/>

#### Delete a fine-tuned model

`Note`: The method in charge of deleting a fine tuned model is found in the `MistralAI.Models.pas` unit.

```Pascal
//uses MistralAI.Models;

  with MistralAI.Models.Delete('Id_Model_to_delete') do
  try
    ShowMessage('Model Deleted');
  finally
    Free;
  end;
```

<br/>
<br/>

### Agents

The official documentation provided by Mistral regarding agents is available [here](https://docs.mistral.ai/capabilities/agents/).

> [!WARNING]
> As of 08/13/2024, only the API for executing an agent is available; however, no API for creating an agent has been made available.  
>
> (See the [***MistralAI.Agents.pas***](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/source/MistralAI.Agents.pas) unit)
>
> To create an agent you must go through the [platform](https://console.mistral.ai/build/agents/new)

> [!TIP]
> The execution of an agent can be done both synchronously and asynchronously. See the class `TAgentRoute` in the **MistralAI.Agents** unit.

<br/>
<br/>

## Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

<br/>
<br/>

## License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.
