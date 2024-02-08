# Delphi MistralAI API

- [Usage](#usage)
    - [Initialization](#initialization)
    - [Models](#models)

## Usage

### Initialization

To initialize API instance you need to [obtain](https://console.mistral.ai) API token from.

Once you have a token, you can initialize `IMistralAI` interface, which is an entry point to the API.

Due to the fact that there can be many parameters and not all of them are required, they are configured using an anonymous function.

```Pascal
uses MistralAI;

var MistralAI: IMistralAI := TMistralAI.Create(API_TOKEN);
```

### Models

List the various models available in the API. You can refer to the Models documentation to understand what models are available.
[Models Documentation] (see https://docs.mistral.ai/models/)

```Pascal
var Models := MistralAI.Models.List;
  try
    for var Model in Models.Data do
      Memo1.Lines.Add(Model.id);
  finally
    Models.Free;
  end;
```

