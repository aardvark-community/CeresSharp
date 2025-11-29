namespace Aardvark.Base

open CeresSharp


[<AutoOpen>]
module ``Rot Extensions`` =

    let private pickle2 (r : Rot2d) = r.Angle
    let private unpickle2 (v : float) = Rot2d(v)
    let private read2 (offset : int) (v : float) = Rot2s(read offset v)
      
    let private pickle3 (r : Rot3d) = r.ToAngleAxis()
    let private unpickle3 (v : V3d) = Rot3d.FromAngleAxis(v)
    let private read3 (offset : int) (v : V3d) = Rot3s(read offset v)

    type Problem with
        member x.AddParameterBlock(rs : Rot3d[]) = x.AddParameterBlock(rs, pickle3, unpickle3, read3)
        member x.AddParameterBlock(rs : Rot2d[]) = x.AddParameterBlock(rs, pickle2, unpickle2, read2)


[<AutoOpen>]
module ``Euclidean Extensions`` =

    [<Struct>]
    type private EuclideanAdapter(rot : V3d, trans : V3d) =
        member x.AngleAxis = rot
        member x.Trans = trans
        
    let private pickle3 (r : Euclidean3d) = EuclideanAdapter(r.Rot.ToAngleAxis(), r.Trans)
    let private unpickle3 (v : EuclideanAdapter) = Euclidean3d(Rot3d.FromAngleAxis(v.AngleAxis), v.Trans)
    let private read3 (offset : int) (v : EuclideanAdapter) = 
        if offset < 0 then
            Euclidean3s(Rot3s(V3s v.AngleAxis), V3s v.Trans)
        else
            Euclidean3s(
                Rot3s(read offset v.AngleAxis), 
                read (offset + 3) v.Trans
            )

    let private read2 (offset : int) (v : Euclidean2d) = 
        if offset < 0 then
            Euclidean2s(Rot2s(scalar v.Rot.Angle), V2s v.Trans)
        else
            Euclidean2s(
                Rot2s(read offset v.Rot.Angle), 
                read (offset + 1) v.Trans
            )

    type Problem with
        member x.AddParameterBlock(rs : Euclidean3d[]) = x.AddParameterBlock(rs, pickle3, unpickle3, read3)
        member x.AddParameterBlock(rs : Euclidean2d[]) = x.AddParameterBlock(rs, read2)


[<AutoOpen>]
module ``Similarity Extensions`` =

    [<Struct>]
    type private Similarity2dAdapter(sqrtScale : float, rot : float, trans : V2d) =
        member x.SqrtScale = sqrtScale
        member x.Angle = rot
        member x.Trans = trans

    [<Struct>]
    type private SimilarityAdapter(sqrtScale : float, rot : V3d, trans : V3d) =
        member x.SqrtScale = sqrtScale
        member x.AngleAxis = rot
        member x.Trans = trans
        
    let private pickle3 (r : Similarity3d) = SimilarityAdapter(sqrt r.Scale, r.Rot.ToAngleAxis(), r.Trans)
    let private unpickle3 (v : SimilarityAdapter) = Similarity3d(v.SqrtScale * v.SqrtScale, Euclidean3d(Rot3d.FromAngleAxis(v.AngleAxis), v.Trans))

    let private read3 (offset : int) (v : SimilarityAdapter) = 
        if offset < 0 then
            Similarity3s(Euclidean3s(Rot3s(V3s v.AngleAxis), V3s v.Trans), scalar v.SqrtScale)
        else
            Similarity3s(
                Euclidean3s(
                    Rot3s(read (offset + 1) v.AngleAxis), 
                    read (offset + 4) v.Trans
                ),
                read offset v.SqrtScale
            )

        
    let private pickle2 (r : Similarity2d) = Similarity2dAdapter(sqrt r.Scale, r.Rot.Angle, r.Trans)
    let private unpickle2 (v : Similarity2dAdapter) = Similarity2d(v.SqrtScale * v.SqrtScale, Euclidean2d(Rot2d(v.Angle), v.Trans))

    let private read2 (offset : int) (v : Similarity2dAdapter) = 
        if offset < 0 then
            Similarity2s(Euclidean2s(Rot2s (scalar v.Angle), V2s v.Trans), scalar v.SqrtScale)
        else
            Similarity2s(
                Euclidean2s(
                    Rot2s(read (offset + 1) v.Angle), 
                    read (offset + 2) v.Trans
                ),
                read offset v.SqrtScale
            )

    type Problem with
        member x.AddParameterBlock(rs : Similarity3d[]) = x.AddParameterBlock(rs, pickle3, unpickle3, read3)
        member x.AddParameterBlock(rs : Similarity2d[]) = x.AddParameterBlock(rs, pickle2, unpickle2, read2)
