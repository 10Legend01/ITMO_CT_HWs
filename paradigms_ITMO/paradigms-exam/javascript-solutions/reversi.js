function Reversi() {
    const n = 8;
    const m = 8;

    this.newBoard = function (count_gamers) {
        let board = [];
        for (let i = 0; i < n; i++) {
            board[i] = [];
            // :NOTE: Array.fill
            for (let j = 0; j < m; j++) {
                board[i][j] = 0;
            }
        }
        for (let i = 0; i < count_gamers; i++) { /// ?
            if (i % 2 === 0) {
                const k = i / 2;
                board[n / 2][m / 2 + k] = i + 1;
                board[n / 2 - 1][m / 2 - 1 - k] = i + 1;
            }
            else {
                const k = parseInt(i / 2);
                board[n / 2][m / 2 - 1 - k] = i + 1;
                board[n / 2 - 1][m / 2 + k] = i + 1;
            }
        }
        return board;
    };

    this.showBoard = function (cboard) {
        let str = "";
        str += ".";
        for (let i = 0; i < cboard.length; i++) {
            str += (i % 10)+"";
        }
        let curr = 0;
        for (let i = 0; i < cboard.length; i++) {
            str += (i % 10)+"";
            for (let j = 0; j < cboard[i].length; j++) {
                str += (cboard[i][j])+"";
            }
            str += "\n";
        }
        console.log(str);
    };

    this.makeBoardOnBoard = function (board) {
        let newBoard = [];
        for (let i = 0; i < board.length; i++) {
            newBoard[i] = [];
            for (let j = 0; j < board[i].length; j++) {
                newBoard[i][j] = board[i][j];
            }
        }
        return newBoard;
    };

    this.checkMove = function (i, j, player, board) {
        let newBoard = this.makeBoardOnBoard(board);
        if (board[i][j] !== 0) {
            return newBoard;
        }


        // :NOTE: * Массовое дублирование
        for (let ci = i, cj = j - 2; -1 < cj; cj--) {
            if (newBoard[ci][cj] === 0 || newBoard[ci][cj+1] === 0 || newBoard[ci][cj+1] === player + 1) break;
            if (newBoard[ci][cj] === player + 1) {
                for (let nj = j; cj < nj; nj--) {
                    newBoard[ci][nj] = player + 1;
                }
                break;
            }
        }

        for (let ci = i - 2, cj = j - 2; -1 < cj && -1 < ci; cj--, ci--) {
            if (newBoard[ci][cj] === 0 || newBoard[ci+1][cj+1] === 0 || newBoard[ci+1][cj+1] === player + 1) break;
            if (newBoard[ci][cj] === player + 1) {
                for (let ni = i, nj = j; cj < nj; nj--, ni--) {
                    newBoard[ni][nj] = player + 1;
                }
                break;
            }
        }

        for (let ci = i - 2, cj = j; -1 < ci; ci--) {
            if (newBoard[ci][cj] === 0 || newBoard[ci+1][cj] === 0 || newBoard[ci+1][cj] === player + 1) break;
            if (newBoard[ci][cj] === player + 1) {
                for (let ni = i; ci < ni; ni--) {
                    newBoard[ni][cj] = player + 1;
                }
                break;
            }
        }

        for (let ci = i - 2, cj = j + 2; cj < newBoard[i].length && -1 < ci; cj++, ci--) {
            if (newBoard[ci][cj] === 0 || newBoard[ci+1][cj-1] === 0 || newBoard[ci+1][cj-1] === player + 1) break;
            if (newBoard[ci][cj] === player + 1) {
                for (let ni = i, nj = j; nj < cj; nj++, ni--) {
                    newBoard[ni][nj] = player + 1;
                }
                break;
            }
        }

        for (let ci = i, cj = j + 2; cj < newBoard[i].length; cj++) {
            if (newBoard[ci][cj] === 0 || newBoard[ci][cj-1] === 0 || newBoard[ci][cj-1] === player + 1) break;
            if (newBoard[ci][cj] === player + 1) {
                for (let nj = j; nj < cj; nj++) {
                    newBoard[ci][nj] = player + 1;
                }
                break;
            }
        }

        for (let ci = i + 2, cj = j + 2; cj < newBoard[i].length && ci < newBoard.length; cj++, ci++) {
            if (newBoard[ci][cj] === 0 || newBoard[ci-1][cj-1] === 0 || newBoard[ci-1][cj-1] === player + 1) break;
            if (newBoard[ci][cj] === player + 1) {
                for (let ni = i, nj = j; nj < cj; nj++, ni++) {
                    newBoard[ni][nj] = player + 1;
                }
                break;
            }
        }

        for (let ci = i + 2, cj = j; ci < newBoard.length; ci++) {
            if (newBoard[ci][cj] === 0 || newBoard[ci-1][cj] === 0 || newBoard[ci-1][cj] === player + 1) break;
            if (newBoard[ci][cj] === player + 1) {
                for (let ni = i; ni < ci; ni++) {
                    newBoard[ni][cj] = player + 1;
                }
                break;
            }
        }

        for (let ci = i + 2, cj = j - 2; -1 < cj && ci < newBoard.length; cj--, ci++) {
            if (newBoard[ci][cj] === 0 || newBoard[ci-1][cj+1] === 0 || newBoard[ci-1][cj+1] === player + 1) break;
            if (newBoard[ci][cj] === player + 1) {
                for (let ni = i, nj = j; cj < ni; nj--, ni++) {
                    newBoard[ni][nj] = player + 1;
                }
                break;
            }
        }
        return newBoard;
    };

    this.canMove = function (player, board) {
        for (let i = 0; i < board.length; i++) {
            for (let j = 0; j < board[i].length; j++) {
                const newBoard = this.checkMove(i, j, player, board);
                if (this.diff(newBoard, board) !== 64) {
                    return true;
                }
            }
        }
        return false;
    };

    this.diff = function (board1, board2) {
        let sum = 0;
        for (let i = 0; i < board1.length; i++) {
            for (let j = 0; j < board1[i].length; j++) {
                if (board1[i][j] === board2[i][j]) {
                    sum++;
                }
            }
        }
        return sum;
    };

    this.makeMove = function (player, type_player, board) {
        if (!this.canMove(player, board)) {
            console.log("Player ", player, " can't move.");
            return board;
        }
        let curr_i, curr_j;
        if (type_player === 0) { // alive
            this.showBoard();
            while (true) {
                curr_i = prompt("Input 'i': ", '-1').trim();
                curr_i = parseInt(curr_i);
                if (curr_i < 0 || curr_i >= board.length) {
                    console.log("Invalid count");
                }
                curr_j = prompt("Input 'j': ", '-1').trim();
                curr_j = parseInt(curr_j);
                if (curr_j < 0 || curr_j >= board[curr_i].length) {
                    console.log("Invalid count");
                }
                const newBoard = this.checkMove(curr_i, curr_j, player, board);
                if (newBoard === false) {
                    console.log("Impossible move");
                }
                this.showBoard(newBoard);
                let str = prompt("Success? y/n: ", "n");
                str.trim();
                if (str === "y") {
                    board = newBoard;
                    break;
                }
            }
        }
        else if (type_player === 1) { // random
            for (let i = 0; i < board.length; i++) {
                let fl = false;
                for (let j = 0; j < board[i].length; j++) {
                    const newBoard = this.checkMove(i, j, player, board);
                    if (this.diff(newBoard, board) !== 64) {
                        board = newBoard;
                        curr_i = i;
                        curr_j = j;
                        fl = true;
                        break;
                    }
                }
                if (fl) {
                    break;
                }
            }
        }
        else if (type_player === 2) { // best
            let best_i, best_j, s = 0;
            for (let i = 0; i < board.length; i++) {
                for (let j = 0; j < board[i].length; j++) {
                    const newBoard = this.checkMove(i, j, player, board);
                    const sum = this.diff(board, newBoard);
                    if (sum !== 64) {
                        if (s < sum) {
                            s = sum;
                            best_i = i;
                            best_j = j;
                        }
                    }
                }
            }
            curr_i = best_i;
            curr_j = best_j;
            board = this.checkMove(best_i, best_j, player);
        }
        console.log("Player ", player, " move on i: ", curr_i, ", j: ", curr_j);
        return board;
    };

    this.endGame = function (board, count_gamers) {
        for (let i = 0; i < count_gamers; i++) {
            if (this.canMove(i, board)) {
                return false;
            }
        }
        return true;
    };

    this.loose = function (count_gamers, board) {
        let ma = [];
        for (let i = 0; i < count_gamers; i++) {
            ma[i] = 0;
        }
        for (let i = 0; i < board.length; i++) {
            for (let j = 0; j < board[i].length; j++) {
                ma[board[i][j]]++;
            }
        }
        let s = m*n;
        let x = -1;
        for (let i = 0; i < count_gamers; i++) {
            if (ma[i] < s) {
                x = i;
                s = ma[i];
            }
        }
        return x;
    };

    this.startGame = function (list_of_gamers) {
        let count_gamers = list_of_gamers.length; // < 9 players
        let board = this.newBoard(count_gamers);
        let player = 0;
        while (!this.endGame(board, count_gamers)) {
            board = this.makeMove(player, list_of_gamers[player], board);
            player = (player + 1) % count_gamers;
        }
        const loose = this.loose(count_gamers, board);
        console.log("Loose player: ", loose);
        return loose;
    }

    /*
    this.tournament = function (list_of_gamers) {

    }*/
}

let game = new Reversi();
game.startGame([1, 1]);