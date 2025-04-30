# Haskell Retrieval & Reranking Service

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

