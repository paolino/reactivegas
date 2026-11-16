export const fetchImpl = (url) => (opts) => (onError) => (onSuccess) => () => {
  fetch(url, {
    method: opts.method,
    headers: { "Content-Type": "application/json" },
    body: opts.body === "" ? undefined : opts.body,
  })
    .then(async (r) => {
      const body = await r.text();
      onSuccess({ status: r.status, body })();
    })
    .catch((e) => {
      onError(e)();
    });
  return () => {};
};
