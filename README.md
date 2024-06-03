# Delphi MistralAI API

___
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.3+-yellow)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-green)


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
- [Contributing](#contributing)
- [License](#license)

## Remarks 

This is an unofficial library. MistralAI does not provide any official library for Delphi.
This repositorty contains Delphi implementation over [MistralAI](https://docs.mistral.ai/api/) public API.
 
## Usage

### Initialization

To initialize API instance you need to [obtain](https://console.mistral.ai/api-keys/) API token from.

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

Codestral is an advanced generative model optimized for code generation, including fill-in-the-middle and code completion. Trained on over 80 programming languages, it performs well on both common and rare languages.
See also [Code generation](https://docs.mistral.ai/capabilities/code_generation/) at the MistralAI web site.

#### Before using

To utilize the Delphi classes managing the Codestral function, you are required to create a new KEY on the Mistral.ai website. Please note that obtaining this key necessitates providing a valid phone number. 
Go to this address to create a key for using Codestral [Key creation](https://console.mistral.ai/codestral/)

## Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

## License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.
