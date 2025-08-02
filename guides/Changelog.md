### 2025, august 3 - version 1.3.0
- Http request monitoring.
- Injectable HTTP Methods (GET, POST, PUT, PATCH) for Simplified Mocking.
- Dunit-Powered Unit Testing for Effortless Execution of All Tests.
- New endpoints enabling an agentic approach:
   - Added `v1/conversations` and `v1/agents`, enabling agentic behaviors and integration with tools such as the code interpreter, web search, and libraries.
- Direct access to documentary content:
   - Introduced `v1/libraries`, `v1/libraries/{library_id}/documents`, and `v1/libraries/{library_id}/share` endpoints to manage, access, and share one or more document resources with fine-grained control.
- Audio processing:
  - Added the `v1/audio/transcriptions` endpoint for transcribing audio files.
- Embedding audio files in chat sessions:
  - Enabled inclusion of audio files directly within a chat session to enrich context or drive processing.
- Optical character recognition (OCR):
  - Provided the `v1/ocr` endpoint to extract information from a wide variety of digital formats.


## 2025, january 2 - version 1.2.0 (Getit version)
- Integrating Moderation Service APIs.
- Integrating Batch Inference.
- Revision of the Simplified Unit Declaration.
- Adding Tools to Simplify the Tutorial.
- Various Code Fixes.
- README.md Revision.