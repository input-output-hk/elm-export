/* @flow */

import Position from 'Position';

export default type FavoritePlaces =
  { positionsByUser: {[string]: (Array<Position>)}
  }
