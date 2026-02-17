type Color = "red" | "green" | "blue";
type RGB = [red: number, green: number, blue: number];
type Palette = Record<Color, string | RGB>;

const palette = {
    red: [255, 0, 0],
    green: "#00ff00",
    blue: [0, 0, 255],
} satisfies Palette;

const a = "hello" satisfies string;

const b = { x: 1 } satisfies Record<string, number>;
