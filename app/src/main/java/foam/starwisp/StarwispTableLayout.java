// Starwisp Copyright (C) 2013 Dave Griffiths
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

package foam.starwisp;

import android.widget.LinearLayout;
import android.widget.TableLayout;
import android.widget.TableRow;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONArray;
import android.view.ViewGroup;
import android.graphics.Color;
import android.util.Log;
import android.util.DisplayMetrics;
import android.util.TypedValue;

public class StarwispTableLayout
{
    public static DisplayMetrics m_DisplayMetrics;

    public static int BuildLayoutParam(String p) {
        if (p.equals("fill-parent")) return TableLayout.LayoutParams.FILL_PARENT;
        if (p.equals("match-parent")) return TableLayout.LayoutParams.MATCH_PARENT;
        if (p.equals("wrap-content")) return TableLayout.LayoutParams.WRAP_CONTENT;
        try {
            return Math.round(TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, Integer.parseInt(p), m_DisplayMetrics));
            //return Integer.parseInt(p);
        } catch (NumberFormatException e) {
            Log.i("starwisp", "Layout error with ["+p+"]");
            // send error message
            return TableLayout.LayoutParams.WRAP_CONTENT;
        }
    }

    public static TableLayout.LayoutParams BuildLayoutParams(JSONArray arr) {
        try {
            float weight = (float)arr.getDouble(3);
            TableLayout.LayoutParams lp;
            if (weight == -1) {
                lp = new TableLayout.LayoutParams(BuildLayoutParam(arr.getString(1)),
						  BuildLayoutParam(arr.getString(2)));
            } else {
                lp = new TableLayout.LayoutParams(BuildLayoutParam(arr.getString(1)),
						  BuildLayoutParam(arr.getString(2)),
						  weight);
            }
            //lp.gravity=BuildLayoutGravity(arr.getString(4));
            int margin=arr.getInt(5);
            lp.setMargins(margin,margin,margin,margin);
            return lp;
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing data " + e.toString());
            return null;
        }
    }

    public static void Build(StarwispBuilder b, final StarwispActivity ctx, final String ctxname, JSONArray arr, ViewGroup parent) {
        try {
            TableLayout v = new TableLayout(ctx);
            v.setId(arr.getInt(1));
            //v.setOrientation(BuildOrientation(arr.getString(2)));
	    TableLayout.LayoutParams tp = BuildLayoutParams(arr.getJSONArray(2));
            v.setLayoutParams(b.BuildLayoutParams(arr.getJSONArray(2))); // maybe should be something else
            //v.setPadding(2,2,2,2);
            JSONArray col = arr.getJSONArray(3);
            v.setBackgroundColor(Color.argb(col.getInt(3), col.getInt(0), col.getInt(1), col.getInt(2)));
            parent.addView(v);
            JSONArray rows = arr.getJSONArray(4);
	    TableRow.LayoutParams rowParams = new TableRow.LayoutParams(TableRow.LayoutParams.WRAP_CONTENT, TableRow.LayoutParams.WRAP_CONTENT);
            for (int i=0; i<rows.length(); i++) {
		Log.i("starwisp","adding tablerow "+i+" to table");
		TableRow tr = new TableRow(ctx);
		v.addView(tr);
		tr.setLayoutParams(rowParams);
		JSONArray children = rows.getJSONArray(i);	    
		for (int c=0; c<children.length(); c++) {
		    Log.i("starwisp","adding child "+c+" to tablerow");
		    b.Build(ctx,ctxname,new JSONArray(children.getString(c)), tr);
		}
            }
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing data in StarwispLinearLayout " + e.toString());
        }
    }

    public static void Update(StarwispBuilder b, TableLayout v, String token, final StarwispActivity ctx, final String ctxname, JSONArray arr) {
    }
}
