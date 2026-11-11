export const getItemImpl = (key) => () => {
  const val = localStorage.getItem(key);
  return val;
};

export const setItemImpl = (key) => (value) => () => {
  localStorage.setItem(key, value);
};

export const removeItemImpl = (key) => () => {
  localStorage.removeItem(key);
};

export const confirmImpl = (msg) => () => {
  return window.confirm(msg);
};

export const copyToClipboardImpl = (text) => () => {
  navigator.clipboard.writeText(text);
};
