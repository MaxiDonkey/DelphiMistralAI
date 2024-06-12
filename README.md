# Delphi MistralAI API

___
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.3+-yellow)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-green)


- [Introduction](#Introduction)
- [Remarks](#remarks)
- [Usage](#usage)
    - [Initialization](#initialization)
    - [Models](#models)
    - [Embeddings](#embeddings)
    - [Chats](#chats)
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
- [Contributing](#contributing)
- [License](#license)

## Introduction

Welcome to the unofficial Delphi **MistralAI** API library. This project aims to provide a `Delphi` interface for interacting with the **MistralAI** public API, making it easier to integrate advanced natural language processing features into your `Delphi` applications. Whether you want to generate text, create embeddings, use chat models, or generate code, this library offers a simple and effective solution.

**MistralAI** is a powerful natural language processing API that enables developers to incorporate advanced AI functionalities into their applications. For more details, visit the [official MistralAI documentation](https://docs.mistral.ai/api/).

## Remarks 

This is an unofficial library. **MistralAI** does not provide any official library for `Delphi`.
This repository contains `Delphi` implementation over [MistralAI](https://docs.mistral.ai/api/) public API.

## Usage

### Initialization

To initialize the API instance, you need to [obtain an API token from MistralAI](https://console.mistral.ai/api-keys/).

Once you have a token, you can initialize `IMistralAI` interface, which is an entry point to the API.

Due to the fact that there can be many parameters and not all of them are required, they are configured using an anonymous function.

```Pascal
uses MistralAI;

var MistralAI: IMistralAI := TMistralAI.Create(API_TOKEN);
```

### Models

List the various models available in the API. You can refer to the Models documentation to understand what models are available.
See [Models Documentation](https://docs.mistral.ai/models/)

```Pascal
//uses MistralAI, MistralAI.Models;

var Models := MistralAI.Models.List;
  try
    for var Model in Models.Data do
      Memo1.Lines.Add(Model.id);
  finally
    Models.Free;
  end;
```

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

### Chats

Using the API to create and maintain conversations. You have the option to either wait for a complete response or receive the response sequentially (Streaming mode).

See also [Prompting Capabilities](https://docs.mistral.ai/guides/prompting_capabilities/) at the MistralAI web site.

```Pascal
//uses MistralAI, MistralAI.Chat;  

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

### Stream mode

```Pascal
//uses MistralAI, MistralAI.Chat;

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

### Function calling

Function calling allows Mistral models to connect to external tools. By integrating Mistral models with external tools such as user defined functions or APIs, users can easily build applications catering to specific use cases and practical problems. 

See also [documentation](https://docs.mistral.ai/capabilities/function_calling/) at the MistralAI web site.

Warning : While this technology is powerful, it also carries potential risks. We strongly advise incorporating user confirmation processes before executing real-world actions on behalf of users, such as sending emails, posting online, making purchases, etc.


```Pascal
//uses 
//  MistralAI, MistralAI.Chat,  
//  MistralAI.Functions.Core, MistralAI.Functions.Example;
  
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

### Code generation

**Codestral** is an advanced generative model optimized for code generation, including **fill-in-the-middle** and code completion. Trained on over 80 programming languages, it performs well on both common and rare languages.
See also [Code generation](https://docs.mistral.ai/capabilities/code_generation/) at the MistralAI web site.

#### Before using

To utilize the Delphi classes managing the **Codestral** function, you are required to create a new KEY on the ***Mistral.ai website***. Please note that obtaining this key necessitates providing a valid phone number. 
Go to this address to create a key for using **Codestral** [Key creation](https://console.mistral.ai/codestral)

#### Codestral initialization

When instantiating the interface managing the ***TMistralAI*** type class, the `CodestralSpec` specification must be specified in the `create` constructor.

The resulting interface will handle both **CodeStral** functionality as well as chat-type interactions.

```Pascal
uses MistralAI;

var CodingModel: IMistralAI := TMistralAI.Create(API_TOKEN, [CodestralSpec]);
```

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

### Fine-tuning

When choosing between prompt engineering and fine-tuning for an AI model, it's advisable to start with prompt engineering due to its speed and lower resource requirements. Fine-tuning, however, offers better performance and alignment with specific tasks, making it ideal for specialized applications and cost reduction.

See also [Fine-tuning description](https://docs.mistral.ai/capabilities/finetuning/) at the **MistralAI** web site.


#### Files

Data should be stored in **JSON** Lines files `(.jsonl)`, where each line corresponds to a separate **JSON object**. This format enables efficient storage of multiple **JSON objects**.

The datasets must adhere to an instruction-following format that simulates a conversation between a user and an assistant. Each JSON record should either contain only messages exchanged between the user and the assistant (referred to as ***"Default Instruct"***), or include additional logic for function calls (referred to as ***"Function-calling Instruct"***). 


#### Create a fine-tuning job

The next step involves creating a fine-tuning job.

- **model**: Select the specific model you wish to fine-tune, with options being "open-mistral-7b" and "mistral-small-latest".
- **training_files**: Provide a set of training file IDs, which can include one or multiple files.
- **validation_files**: Provide a set of validation file IDs, which can include one or multiple files.
- **hyperparameters**: Adjust the two available hyperparameters, "trainingₛtep" and "learning_rate", according to your preferences.


## Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

## License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.
