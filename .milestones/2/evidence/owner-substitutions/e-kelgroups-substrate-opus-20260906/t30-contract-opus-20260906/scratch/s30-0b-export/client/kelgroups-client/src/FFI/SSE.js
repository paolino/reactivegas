export const createImpl = (url) => () => {
  return new EventSource(url);
};

export const onMessageImpl = (es) => (handler) => () => {
  es.addEventListener("new", (e) => {
    handler(e.data)();
  });
};

export const close = (es) => () => {
  es.close();
};
