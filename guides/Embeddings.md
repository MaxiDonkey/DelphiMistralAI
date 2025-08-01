# Embeddings (v1/embeddings)

- [Introduction](#introduction)
- [Text embeddings](#text-embeddings)
- [Code Embeddings](#code-embeddings)

<br>

___

## Introduction

Embeddings make it possible to vectorize one or more texts in order, for example, to calculate the similarity between sentences. Each vector resulted will be of dimension 1024. This vector representation captures deep semantic aspects of texts, allowing for more nuanced comparisons.
Distance measures such as cosine, Euclidean distance or other custom measures can be applied to these embeddings. 

See also [tokenization](https://docs.mistral.ai/guides/tokenization/) at the MistralAI web site.

<br>

___

## Text embeddings

The Mistral AI embeddings API lets you convert text into vector representations. You send a request with the list of texts to process and specify the `mistral-embed` model. The service returns a series of embeddings—numeric vectors—that can be used as the foundation for semantic analysis, indexing, or any other advanced logic within an NLP processing pipeline.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Embeddings.AsyncAwaitCreate(
    procedure (Params: TEmbeddingParams)
    begin
      Params.Model('mistral-embed');
      Params.Input([
        'Text to vectorize'
      ]);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<TEmbeddings>(
      function (Value: TEmbeddings): TEmbeddings
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
//  Client.Embeddings.AsyncCreate(
//    procedure (Params: TEmbeddingParams)
//    begin
//      Params.Model('mistral-embed');
//      Params.Input([
//        'Text to vectorize'
//      ]);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//
//    function : TAsyncEmbeddings
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Embeddings := Client.Embeddings.Create(
//    procedure (Params: TEmbeddingParams)
//    begin
//      Params.Model('mistral-embed');
//      Params.Input([
//        'Text to vectorize'
//      ]);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, Embeddings);
//  finally
//    Embeddings.Free;
//  end;
```

<br>

___

## Code Embeddings

To produce code embeddings with Mistral AI’s `embeddings` API, send a request to the endpoint specifying the codestral-embed model and supplying the texts to embed. The API responds with numerical vector representations for each input, which can be consumed downstream in NLP workflows (e.g., similarity search, indexing, semantic analysis).

Two optional parameters let you tailor the output:

output_dtype controls the numeric format and precision of the embeddings:

- **float (default):** 32-bit single-precision floats, offering the highest fidelity and retrieval quality.

- **int8:** Signed 8-bit integers (−128 to 127).

- **uint8:** Unsigned 8-bit integers (0 to 255).

- **binary:** Bit-packed, quantized single-bit embeddings stored as int8; length is output_dimension / 8. Uses offset binary encoding.

- **ubinary:** Like binary but uses uint8 as the underlying container.

`output_dimension` lets you specify the embedding size. It defaults to 1536 and can be increased up to 3072. When you request a target dimension n, the API returns the first n components of the full embedding; those dimensions are ordered by relevance to provide a controlled quality/cost trade-off.

<br>

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  var problem_description :=
    'Given an array of integers nums and an integer target, return indices of the two numbers '+
    'such that they add up to target. You may assume that each input would have exactly one '+
    'solution, and you may not use the same element twice. You can return the answer in any '+
    'order. Example 1: Input: nums = [2,7,11,15], target = 9 Output: [0,1] Explanation: Because '+
    'nums[0] + nums[1] == 9, we return [0, 1]. Example 2: Input: nums = [3,2,4], target = 6 Output: '+
    '[1,2] Example 3: Input: nums = [3,3], target = 6 Output: [0,1] Constraints: 2 <= nums.length <= '+
    '104 -109 <= nums[i] <= 109 -109 <= target <= 109 Only one valid answer exists.';

  var solution :=
    'class Solution: def twoSum(self, nums: List[int], target: int) -> List[int]: d = {} for i, '+
    'x in enumerate(nums): if (y := target - x) in d: return [d[y], i] d[x] = i';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Embeddings.AsyncAwaitCreate(
    procedure (Params: TEmbeddingParams)
    begin
      Params.Model('codestral-embed');
      Params.output_dimension(10);
      Params.output_dtype('binary');
      Params.Input([problem_description, solution]);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<TEmbeddings>(
      function (Value: TEmbeddings): TEmbeddings
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
//  Client.Embeddings.AsyncCreate(
//    procedure (Params: TEmbeddingParams)
//    begin
//      Params.Model('codestral-embed');
//      Params.output_dimension(10);
//      Params.output_dtype('binary');
//      Params.Input([problem_description, solution]);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//
//    function : TAsyncEmbeddings
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Embeddings := Client.Embeddings.Create(
//    procedure (Params: TEmbeddingParams)
//    begin
//      Params.Model('codestral-embed');
//      Params.output_dimension(10);
//      Params.output_dtype('binary');
//      Params.Input([problem_description, solution]);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, Embeddings);
//  finally
//    Embeddings.Free;
//  end;
```