import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

public class FrogViewer extends JPanel {
	public static final int DIM = 30;
	public static final int SIZE = 20; // size of each square (px)
	FrogViewer() {
		addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				System.out.println("Row " + e.getY() / SIZE + "\tCol " + e.getX() / SIZE);
			}
		});
	}
	// 1: blue and green components
	// 2: red components
	// 3: dots, blue and green
	// 4: dots, red
	// mod3 is set freely, who cares
	private float[][][] data = new float[5][DIM][DIM];
	private int mod3;
	private int b(boolean damp, float f) { 
		if (damp) {
			return (int) (127 + 128 * (1 - f));
		} else {
			return (int) (255 * (1 - f));
		}
	}
	private boolean slant = false;
	private boolean dotsActivated = false;

	private Color getColorAt(int r, int c, boolean damp) {
		return new Color(
				b(damp, data[2][r][c]),
				b(damp, data[1][r][c]),
				b(damp, data[1][r][c]));
	}
	private Color getDotColorAt(int r, int c) {
		return new Color(
				b(false, data[4][r][c]),
				b(false, data[3][r][c]),
				b(false, data[3][r][c]));
	}

	public static int mod(int a, int b) {
		int s = a % b; return s < 0 ? s + b : s;
	}
	private static Color[] makeColors() {
		Color[] cs = new Color[3];
		cs[0] = new Color(0, 0, 255);
		cs[1] = new Color(0, 128, 255);
		cs[2] = new Color(128, 0, 255);
		return cs;
	}
	private static Color[] drawColors = makeColors();
	private static Color secondaryColor = new Color(0, 128, 255);
	public void paintComponent(Graphics g) {
		for (int r = 0; r < DIM; r++) {
			for (int c = 0; c < DIM; c++) {
				g.setColor(getColorAt(r, c, dotsActivated));
				g.fillRect((slant ? c + DIM - r - 1 : c) * SIZE, r * SIZE, SIZE, SIZE);
				if (dotsActivated) {
					g.setColor(getDotColorAt(r, c));
					g.fillOval((slant ? c + DIM - r - 1 : c) * SIZE + 3, r * SIZE + 3, SIZE - 6, SIZE - 6);
				}
				switch (mod(r - c + mod3 + 1, 3)) {
					case 2:
						switch (mod(r + c + 2, 3)) {
							case 2:
							case 1:
								g.setColor(drawColors[0]);
								g.drawRect((slant ? c + DIM - r - 1 : c) * SIZE + 2, r * SIZE + 2, SIZE - 5, SIZE - 5);
								break;
							case 0:
								g.setColor(drawColors[1]);
								g.drawRect((slant ? c + DIM - r - 1 : c) * SIZE + 4, r * SIZE + 4, SIZE - 9, SIZE - 9);
								break;
						}
						break;
					case 1:
						switch (mod(r + c + 2, 3)) {
							case 2:
								g.setColor(drawColors[1]);
								g.drawRect((slant ? c + DIM - r - 1 : c) * SIZE + 4, r * SIZE + 4, SIZE - 9, SIZE - 9);
								break;
							case 1:
								g.setColor(drawColors[2]);
								g.drawRect((slant ? c + DIM - r - 1 : c) * SIZE + 6, r * SIZE + 6, SIZE - 13, SIZE - 13);
								break;
						}
						break;
				}
			}
		}
	}
	private static void readSessionIntoArray(float[][] array, int session) throws IOException {
		if (session == 0) {
			// special value
			for (int r = 0; r < DIM; r++) {
				for (int c = 0; c < DIM; c++) {
					array[r][c] = 0;
				}
			}
			return;
		}
		File inf = new File("s" + session + ".txt");
		if (inf.exists()) {
			BufferedReader rd = new BufferedReader(new FileReader(inf));
			for (int r = 0; r < DIM; r++) {
				String s = rd.readLine();
				int c = 0;
				for (String tok : s.split(",")) {
					array[r][c++] = Float.parseFloat(tok);
				}
			}
		} else {
			throw new IllegalArgumentException("no file for session " + session);
		}
	}
	private static void copyArray(float[][] src, float[][] dest) {
		for (int r = 0; r < DIM; r++) {
			for (int c = 0; c < DIM; c++) {
				dest[r][c] = src[r][c];
			}
		}
	}
	public void readSession(char flag, int session) throws IOException {
		readSessionIntoArray(data[0], session);
		switch (flag) {
			case 'a':
				dotsActivated = false;
				copyArray(data[0], data[1]);
				copyArray(data[0], data[2]);
				mod3 = session % 3;
				break;
			case 'r':
				dotsActivated = false;
				copyArray(data[0], data[2]);
				mod3 = session % 3;
				break;
			case 'd':
				dotsActivated = true;
				copyArray(data[0], data[3]);
				copyArray(data[0], data[4]);
				mod3 = session % 3;
				break;
			case 'D':
				dotsActivated = true;
				copyArray(data[0], data[4]);
				mod3 = session % 3;
				break;
			case 'z':
				// cheap hack
				dotsActivated = true;
				copyArray(data[0], data[1]);
				copyArray(data[0], data[2]);
				readSessionIntoArray(data[3], session + 1);
				readSessionIntoArray(data[4], session + 301);
				mod3 = (session + 1) % 3;
				break;
			case 'x':
				// cheap hack
				dotsActivated = true;
				copyArray(data[0], data[1]);
				copyArray(data[0], data[2]);
				readSessionIntoArray(data[3], session + 1);
				readSessionIntoArray(data[4], session + 601);
				mod3 = (session + 1) % 3;
				break;
			case 'c':
				// cheap hack
				dotsActivated = true;
				copyArray(data[0], data[1]);
				copyArray(data[0], data[2]);
				readSessionIntoArray(data[3], session + 1);
				readSessionIntoArray(data[4], session + 901);
				mod3 = (session + 1) % 3;
				break;
			default:
				throw new IllegalArgumentException("invalid char " + flag);
		}
		repaint();
	}
	public static void main(String[] args) throws IOException{
		FrogViewer v = new FrogViewer();

		if (args.length >= 1 && args[0].equals("slant")) v.slant = true;

		v.readSession('a', 1);

		JFrame f = new JFrame();
		f.add(v);
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		f.setSize(650, 650);
		f.setVisible(true);

		Scanner scanner = new Scanner(System.in);
		while (scanner.hasNextLine()) {
			try {
				String t = scanner.nextLine();
				int session = Integer.parseInt(t.substring(1));
				v.readSession(t.charAt(0), session);
			} catch (Exception e) {
				System.out.println("Exception: " + e.getMessage());
			}

		}
		System.exit(0);
	}

}
