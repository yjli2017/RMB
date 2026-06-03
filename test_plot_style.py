import unittest

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from rmb.visualizer import NATURE_PALETTE, apply_nature_style, save_nature_figure


class NatureStyleTests(unittest.TestCase):
    def test_nature_palette_is_colorblind_safe_and_named(self):
        self.assertIn("blue", NATURE_PALETTE)
        self.assertIn("vermillion", NATURE_PALETTE)
        self.assertIn("sky", NATURE_PALETTE)
        self.assertEqual(NATURE_PALETTE["blue"], "#0072B2")

    def test_apply_nature_style_removes_top_right_spines_and_uses_subtle_grid(self):
        fig, ax = plt.subplots()
        ax.plot([0, 1], [0, 1])
        apply_nature_style(ax)
        self.assertFalse(ax.spines["top"].get_visible())
        self.assertFalse(ax.spines["right"].get_visible())
        self.assertTrue(ax.yaxis._major_tick_kw["gridOn"])
        plt.close(fig)

    def test_save_nature_figure_writes_high_dpi_png(self):
        fig, ax = plt.subplots()
        ax.plot([0, 1], [0, 1])
        out = "/tmp/rmb_nature_style_test.png"
        save_nature_figure(fig, out, dpi=220)
        self.assertTrue(__import__("pathlib").Path(out).exists())
        plt.close(fig)


if __name__ == "__main__":
    unittest.main()
