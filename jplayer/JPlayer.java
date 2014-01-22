package jplayer;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

public class JPlayer {
	public JPlayer() {
	}

	// 0 1 2
	// 3 4 5
	// 6 7 8
	public Move getMoveExample(ArrayList<ArrayList<Square>> board, int activeBoard, boolean isP1Turn, ArrayList<Cell> p1Wins, ArrayList<Cell> p2Wins, ArrayList<Cell> draws) {
		// char iAm = isP1Turn ? 'X' : 'O';
		int key = 0;

		if (activeBoard == -1) {
			Map<Integer, Move> emptySquares = new HashMap<Integer, Move>();

			// random empty square
			for (int i = 0; i < 9; ++i) {
				for (int j = 0; j < 9; ++j) {
					if (board.get(i).get(j).square == ' ') {
						emptySquares.put(key, new Move(i, j));
						key += 1;
					}
				}
			}

			int randomGuess = (new Random()).nextInt(key);
			return emptySquares.get(randomGuess);
		}

		List<Square> squares = board.get(activeBoard);

		// center ?
		if (squares.get(4).square == ' ')
			return new Move(activeBoard, 4);

		Map<Integer, Move> corners = new HashMap<Integer, Move>();

		// corner ?
		if (squares.get(0).square == ' ') {
			corners.put(key, new Move(activeBoard, 0));
			key += 1;
		}
		if (squares.get(2).square == ' ') {
			corners.put(key, new Move(activeBoard, 2));
			key += 1;
		}
		if (squares.get(6).square == ' ') {
			corners.put(key, new Move(activeBoard, 6));
			key += 1;
		}
		if (squares.get(8).square == ' ') {
			corners.put(key, new Move(activeBoard, 8));
			key += 1;
		}

		if (corners.size() != 0) {
			int randomGuess = (new Random()).nextInt(corners.size());
			return corners.get(randomGuess);
		}

		Map<Integer, Move> edges = new HashMap<Integer, Move>();

		// ... edge
		if (squares.get(1).square == ' ') {
			edges.put(key, new Move(activeBoard, 1));
			key += 1;
		}
		if (squares.get(3).square == ' ') {
			edges.put(key, new Move(activeBoard, 3));
			key += 1;
		}
		if (squares.get(5).square == ' ') {
			edges.put(key, new Move(activeBoard, 5));
			key += 1;
		}
		if (squares.get(7).square == ' ') {
			edges.put(key, new Move(activeBoard, 7));
			key += 1;
		}

		int randomGuess = (new Random()).nextInt(edges.size());
		return edges.get(randomGuess);
	}
}
