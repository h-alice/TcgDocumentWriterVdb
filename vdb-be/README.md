# Retrieval & Reranking Service

## Description

This project provides a high-performance Haskell web service built with the Warp server. It's designed to act as a backend component that:

1.  Receives a user query and parameters via a JSON API endpoint.
2.  Performs an initial candidate document retrieval from a **Weaviate** vector database instance using hybrid search (combining vector and keyword search).
3.  Sends the retrieved documents to an external **Reranker** service API.
4.  Re-sorts the documents based on the relevance scores returned by the reranker.
5.  Returns the final list of top-ranked document contents to the client.

The service is configurable via environment variables and includes basic health checks.

## Features

* **Hybrid Retrieval:** Fetches an initial set of candidate documents from Weaviate using configurable hybrid search parameters (`alpha`, `poolSize`).
* **External Reranking:** Improves result relevance by sending candidate documents to a dedicated reranker microservice API.
* **JSON API:** Exposes a primary endpoint (`/retrieval`) for performing the retrieve-and-rerank operation.
* **Health Check:** Includes a standard `/health` endpoint for monitoring service status.
* **Configurable:** Key parameters like service port and external API endpoints are configured via environment variables.
* **Asynchronous & Performant:** Built using Haskell's high-performance Warp server and libraries like Aeson and Wreq.

## Requirements
* **GHC:** Glasgow Haskell Compiler (e.g., version 9.6+ recommended).
* **Cabal:** Haskell build tool (e.g., version 3.12+) or **Stack**.
* **Running Weaviate Instance:** Access to a Weaviate database populated with the collections you intend to query.
* **Running Reranker Service:** Access to a reranking microservice compatible with LLaMa.CPP.

## Configuration

The service is configured using environment variables at startup:

* `WEAVIATE_ENDPOINT` **(Required)**: The full URL of your Weaviate instance's GraphQL endpoint (e.g., `http://localhost:8080/v1/graphql`). The server will **fail to start** if this is not set.
* `RERANKER_ENDPOINT` **(Required)**: The full URL of your Reranker service endpoint (e.g., `http://localhost:8089/v1/rerank`). The server will **fail to start** if this is not set.
* `SERVER_PORT` (Optional): The port number the service should listen on.
    * If not set or invalid, a **warning** will be printed, and it will default to `3000`.

## Building

Use Cabal or Stack to build the project:

```bash
# Using Cabal
cabal build

# Using Stack
stack build
```

## Running

Ensure the required environment variables are set before running:

```bash
# Example using bash/zsh
export WEAVIATE_ENDPOINT="http://your-weaviate-host:8080/v1/graphql"
export RERANKER_ENDPOINT="http://your-reranker-host:8089/v1/rerank"
# export SERVER_PORT=8000 # Optional

# Using Cabal
cabal run rr-server

# Using Stack
stack exec rr-server
```

The server will print log messages indicating the configuration loaded and the port it's listening on.

## API Endpoints

### 1. Retrieval Endpoint

* **Path:** `/retrieval`
* **Method:** `POST`
* **Description:** Performs the core retrieve-and-rerank workflow. Takes user query and parameters, interacts with Weaviate and the Reranker, and returns the final sorted documents.
* **Request Body:** `application/json`

    ```json
    {
      "requestId": "user-request-id-123",
      "query": "What is black bulbul?",
      "collection": "Document",
      "queryParams": {
        "topK": 5,
        "poolSize": 50,
        "alpha": 0.5
      }
    }
    ```
    * `requestId` (Text): An arbitrary identifier provided by the client, echoed back in the response.
    * `query` (Text): The user's search query.
    * `collection` (Text): The name of the Weaviate collection to search within.
    * `queryParams` (Object): Parameters controlling the retrieval process.
        * `topK` (Int): The final maximum number of documents to return after reranking.
        * `poolSize` (Int): The number of candidate documents to fetch initially from Weaviate (should be >= `topK`).
        * `alpha` (Double): The weighting for Weaviate's hybrid search (0.0 = pure keyword, 1.0 = pure vector, 0.5 = balanced).

    *(Note: The exact structure of `queryParams` depends on the `RetrievalParameters` type defined in `Core.hs`)*

* **Success Response (200 OK):** `application/json`

    ```json
    {
      "requestId": "user-request-id-123",
      "documents": [
        "The black bulbul (Hypsipetes leucocephalus), also known as the Himalayan black bulbul...",
        "It is found primarily in the Himalayas, its range stretching from Pakistan eastward...",
        "The black bulbul is a member of the bulbul family of passerine birds."
      ]
    }
    ```
    * `requestId` (Text): The same ID provided in the request.
    * `documents` ([Text]): A list of the top `k` document contents, sorted according to the reranker's scores.

* **Error Responses:**
    * `400 Bad Request`: If the request JSON is malformed or missing required fields. The response body contains a JSON object with an `"error"` message.
    * `405 Method Not Allowed`: If a method other than POST is used for `/retrieval`.
    * `500 Internal Server Error`: If an error occurs during communication with Weaviate or the Reranker service, or during processing. The response body contains a JSON object with the `requestId` and an `"error"` message.

### 2. Health Check Endpoint

* **Path:** `/health`
* **Method:** `GET`
* **Description:** A simple endpoint to check if the service is running.
* **Success Response (200 OK):** `text/plain`
    * Body: `OK`
* **Error Responses:**
    * `405 Method Not Allowed`: If a method other than GET is used.

### Other Endpoints

* `GET /`: Returns a simple informational message.
* `ANY /teapot`: Returns `418 I'm a teapot`.
* Any other path: Returns `404 Not Found`.

## License

This project is licensed under the **AGPL-3.0**. See the `LICENSE` file for details.
```