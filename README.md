# Delphi MistralAI API

- [Usage](#usage)


## Usage

### Initialization

To initialize API instance you need to [obtain](https://auth.mistral.ai/) API token from.

Once you have a token, you can initialize `IMistralAI` interface, which is an entry point to the API.

Due to the fact that there can be many parameters and not all of them are required, they are configured using an anonymous function.

```Pascal
uses MistralAI;

var MistralAI: IMistralAI := TMistralAI.Create(API_TOKEN);
```
