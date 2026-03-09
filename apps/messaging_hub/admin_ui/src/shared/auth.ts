export const API_KEY_STORAGE_KEY = "MESSAGING_HUB_API_KEY";
export const API_ERROR_EVENT = "messaging-hub-api-error";

export interface ApiErrorEventDetail {
  status: number;
  url: string;
  message: string;
}

export function getMessagingHubApiKey(): string {
  return (
    window.localStorage.getItem(API_KEY_STORAGE_KEY) ??
    new URLSearchParams(window.location.search).get("api_key") ??
    ""
  );
}
